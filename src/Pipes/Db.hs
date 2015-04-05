-- TODO ensure this handles hidden cases
{-# LANGUAGE OverloadedStrings #-}
module Pipes.Db (pipe_ConfFp) where

import           Control.Monad
import           Control.Monad.M
import           Control.Monad.State
import           Data.Either
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Distribution.Package as C
import           Filesystem
import qualified Filesystem.Path.CurrentOS as P

import           Options.DbProvider
import           Package (unversioned)
import           Pipes
                 
-- imports necessary for working with Cabal package db tools 

import Distribution.Package

import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.PackageIndex
       
-- | A totally arbitrary program DB to pass to cabal.
-- This is just necessary to call ghc-pkg.
-- TODO
--  I'm not going to support variations and detection 
--  of 'ghc-pkg', analogous to how cabal calls it right now. 
--  If you really want that, then please submit a branch or PR. 
--    -JPF

fromIndex :: 
   InstalledPackageIndex 
   -> State [C.Dependency] (Maybe (String, PackageIdentifier)) 
fromIndex index = undefined 

defaultGhcProgDb :: ProgramDb
defaultGhcProgDb = addKnownProgram ghcPkgProgram emptyProgramDb

isPackage :: String -> Bool
isPackage str = str /= "    (no packages)" && "   " `L.isPrefixOf` str 

-- | If parsed matches a member of the set by version first, return,
-- otherwise return unversioned match.
toPkgMatch :: String -> State (S.Set String) (Maybe (String, PackageIdentifier)) 
toPkgMatch parsed = do
  unassigned <- get 
  if S.member parsed unassigned -- found versioned 
    then fromFound parsed
  else 
    let parsed' = unversioned parsed in
      if S.member parsed' unassigned 
        then fromFound parsed'
        else return Nothing 
  where
    fromFound :: String -> State (S.Set String) (Maybe (String, PackageIdentifier))
    fromFound p = do
      modify (S.delete p)
      return . Just $ (p, read parsed)

-- | A crude parser that extracts db -> package set mappings based
-- on whitespace indentation 
-- fromOutput :: 
--   [String] -- output of ghc-pkg or ghc-pkg like listing
--   -> S.Set String -- set of packages not yet associated to a db 
--   -> [(FilePath, [(String, PackageIdentifier)])]
--   -> M [(FilePath, [(String, PackageIdentifier)])]
-- fromOutput [] _ assigned = 
--   return assigned 
-- fromOutput (l:rest) unassoc assoc = 
--   if S.null unassoc then -- we're done 
--     return assoc
--   else
--     case assoc of 
--       []     -> 
--         if isPath l then
--           fromOutput rest unassoc [(toPath l, [])] 
--         else
--           err "parse error on package list output, check program used" 
--       ((db, members):dbs) ->
--         uncurry (fromOutput rest) $ 
--           if isPath l then -- it's another db
--             (,) unassoc ((toPath l, []):assoc)
--           else -- it could be a package 
--             if isPackage l then
--               let 
--                 (r, unassoc') =
--                   runState (toPkgMatch (unhide $ drop 4 l)) unassoc
--               in (,) unassoc' $ 
--                    maybe 
--                      assoc -- no change 
--                      (\p -> (db, p : members) : dbs) -- at to members 
--                      r
--             else
--               (unassoc, assoc)
--   where
--     unhide :: String -> String
--     unhide ('(':str) = L.take (L.length str - 1) str 
--     unhide s         = s 

--     -- | remove last ':' and junk 
--     -- Interestingly, when running cabal and ghc proc, its stdout actually 
--     -- appends colons to paths, instead of what appears on console.
--     -- So we're stripping them blindly for now.
--     toPath :: String -> String
--     toPath s = L.take (L.length s - 1) s

--     isPath :: String -> Bool
--     isPath [] = False
--     isPath s  = head s `L.notElem`  "\n\r\t "

-- FIXME 
toMapping :: 
  String ->
  [String] ->
  [C.Dependency] -> 
  M [(FilePath, [(String, PackageIdentifier)])]
toMapping cmd args deps = undefined 
  -- res <- liftIO $ readProcessWithExitCode cmd args []
  -- case res of 
  --   (ExitFailure _, out, err') ->
  --     err $ 
  --       "failed to retrieve package db's from " ++ cmd ++", output:"
  --       ++ out ++ '\n':err' 
  --   (ExitSuccess, out, _) ->
  --     fromOutput (L.lines out) pkgs []

-- TODO 
-- select between cabal infrastructure res. and ghc package dir resolution 

-- | Weakest pre-condition: dependency list is version disjoint.
fromProvider :: 
  DbProvider -> [C.Dependency] -> M [(FilePath, [(String, PackageIdentifier)])] 
  -- ^ (database dir, [package string -> packageId]) 
fromProvider prov pkgs = do
  case prov of 
    (Ghc _)          -> toMapping cmd extra_args pkgs 
    (CabalSandbox _) -> toMapping cmd extra_args pkgs
    (Db fp)          -> fromProvider (Ghc . Just $ "--package-db=" ++ fp) pkgs
  where
    (cmd , extra_args) = toExec prov

isConf :: PackageIdentifier -> P.FilePath -> Bool 
isConf p f = P.hasExtension f "conf" && pkgRelated p f
  
findConf :: PackageIdentifier -> State (S.Set P.FilePath) (Either PackageIdentifier P.FilePath)
findConf package = do
  files <- get 
  let matching = S.filter (isConf package) files
  if not . S.null $ matching
    then let found = S.findMin matching in do
      modify (S.delete (S.findMin matching))
      return $ Right found 
    else
      return $ Left package 
  
-- | Return a list of package configurations for the given
-- db and handled packages
fromPair :: FilePath -> [PackageIdentifier] -> M [FilePath]
fromPair _ []      =
  return [] 
fromPair db members = do 
  confs <- S.fromList <$> liftIO (listDirectory (P.decodeString db))
  let (remainder, confs') = partitionEithers $ evalState (mapM findConf members) confs 
  unless (L.null remainder) . warning $ 
    "The following packages were not found in the pkg db dir: \n" ++  
    L.intercalate "\n" (map show remainder)
  mapM (return . P.encodeString) confs'

pkgRelated :: PackageIdentifier -> P.FilePath -> Bool
pkgRelated p = 
  T.isPrefixOf (T.pack . show $ p) 
    . T.pack 
    . P.encodeString 
    . P.filename

-- | Pre-condition:
-- Awaited dependencies are version disjoint (by Cabal VersionRange type)
pipe_ConfFp :: DbProvider -> PipeM [C.Dependency] FilePath ()
pipe_ConfFp prov = do 
  dependencies <- await 
  if L.null dependencies 
    then  
      lift . err $ "no results possible due to no packages provided"  
    else do
      lift $ do
        msg "db provider:"
        indentM 2 $ msg . show $ prov 
        msg "\n"
      
      pairings <- lift $ fromProvider prov dependencies

      let found   = undefined 
          unfound = undefined
      -- let found = S.unions $ map (S.fromList . map fst . snd) pairings 
      --     unfound = S.difference dependencies found 

      if not . S.null $ unfound then
        lift . err $ 
          L.intercalate "\n"  
            ("The following packages were not found in searched package db's:" 
            : S.toList unfound)

      else -- yield over each returned file,
           --  types are added to make this _much_ easier to understand  
        let 
          mapped :: (FilePath, [(String, PackageIdentifier)]) -> M [FilePath]
          mapped = uncurry fromPair . (\(db, members) -> (db, map snd members))
        in 
          mapM_ yield =<< lift (L.concat <$> mapM mapped pairings)
