-- TODO ensure this handles hidden cases
{-# LANGUAGE OverloadedStrings #-}
module Pipe.Db (pipe_ConfFp) where
import           Pipes

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

import           Options.DbStack
                 
-- imports necessary for working with Cabal package db tools 

import Distribution.Package
import Distribution.Simple.Program      as C
import Distribution.Simple.Program.Db   as CD
import Distribution.Simple.PackageIndex as CI 
import Distribution.Simple.Compiler     as CC
import Distribution.Verbosity           as CVB
import Distribution.Version             as CVS

-- f :: DbStack -> PackageDBStack
-- f (Sandbox   _) = [SpecificPackageDB] 
-- f (Ghc       _) = [UserPackageDB, GlobalPackageDB] 
-- f (Single path) = [SpecificPackageDB path] 

toIndex :: IO InstalledPackageIndex 
toIndex = do
 result <- 
   -- FIXME anyVersion is probably wrong, as the functional dep. 
   -- for operated packages is as follows: 
   -- haddock-api -> compiled ghc version -> ghc-pkg version -> ghc package 

   C.lookupProgramVersion normal ghcPkgProgram anyVersion 
    $ addKnownProgram ghcPkgProgram emptyProgramDb
 case result of 
   Left  err                                -> undefined 
   Right (cfd_program, version, program_db) -> undefined
 
-- fromArgs :: String -> ConfiguredProgram -> ConfigureProgram
-- fromArgs []   cfd_program = cfd_program
-- fromArgs args cfd_program = 

-- fromIndex :: 
--    InstalledPackageIndex 
--    -> State [C.Dependency] (Maybe (String, PackageIdentifier)) 
-- fromIndex index = undefined 

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

fromProvider :: 
  DbStack -> [C.Dependency] -> M [(FilePath, [(String, PackageIdentifier)])] 
  -- ^ (database dir, [package string -> packageId]) 
fromProvider prov pkgs =
  case prov of 
    (Ghc     _) -> toMapping cmd extra_args pkgs
    (Sandbox _) -> toMapping cmd extra_args pkgs
    (Single fp) -> fromProvider (Ghc . Just $ "--package-db=" ++ fp) pkgs
  where
    (cmd , extra_args) = undefined

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

pipe_ConfFp :: DbStack -> PipeM [C.Dependency] FilePath ()
pipe_ConfFp stack = do 
  dependencies <- await 
  if L.null dependencies 
    then  
      lift . err $ "no results possible due to no packages provided"  
    else do
      lift $ do
        msg "db provider:"
        -- indentM 2 $ msg . show $ prov 
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
