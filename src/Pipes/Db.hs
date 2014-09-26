{-# LANGUAGE OverloadedStrings #-}
module Pipes.Db (pipe_ConfFp) where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.M
import           Control.Monad.State
import           Data.Either
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import           Filesystem
import qualified Filesystem.Path.CurrentOS as P
import qualified Module as Ghc
import           Options.DbProvider
import           Package (unversioned)
import           Pipes
import           System.Exit
import           System.Process

isPackage :: String -> Bool
isPackage str = str /= "    (no packages)" && "   " `L.isPrefixOf` str 

-- | If parsed matches a member of the set by version first, return,
-- otherwise return unversioned match.
toPkgMatch :: String -> State (S.Set String) (Maybe (String, Ghc.PackageId)) 
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
    fromFound :: String -> State (S.Set String) (Maybe (String, Ghc.PackageId))
    fromFound p = do
      modify (S.delete p)
      return . Just $ (p, Ghc.stringToPackageId parsed)

-- | A crude parser that extracts db -> package set relations based
-- on whitespace indentation 
accumPaths :: 
  [String] -- output of ghc-pkg or ghc-pkg like listing
  -> S.Set String -- set of packages not yet associated to a db 
  -> [(FilePath, [(String, Ghc.PackageId)])]
  -> M [(FilePath, [(String, Ghc.PackageId)])]
accumPaths [] _ assigned = 
  return assigned 
accumPaths (l:rest) unassigned assigned = 
  if S.null unassigned then -- we're done 
    return assigned
  else
    case assigned of 
      []     -> 
        if isPath l then
          accumPaths rest unassigned [(toPath l, [])] 
        else
          err "parse error on package list output, check program used" 
      ((db, members):dbs) ->
        uncurry (accumPaths rest) $ 
          if isPath l then -- it's another db
            (,) unassigned ((toPath l, []):assigned)
          else -- it could be a package 
            if isPackage l then
              let 
                (r, unassigned') =
                  runState (toPkgMatch (unhide $ drop 4 l)) unassigned
              in (,) unassigned' $ 
                   maybe 
                     assigned -- no change 
                     (\p -> (db, p : members) : dbs) -- at to members 
                     r
            else
              (unassigned, assigned)
  where
    unhide :: String -> String
    unhide ('(':str) = L.take (L.length str - 1) str 
    unhide s         = s 

    -- | remove last ':' and junk 
    -- Interestingly, when running cabal and ghc proc, it's stdout actually 
    -- appends colons to paths, instead of what appears on console.
    -- So we're stripping them blindly for now.
    toPath :: String -> String
    toPath s = L.take (L.length s - 1) s

    isPath :: String -> Bool
    isPath [] = False
    isPath s  = head s `L.notElem`  "\n\r\t "

toMapping :: String -> [String] -> S.Set String -> M [(FilePath, [(String, Ghc.PackageId)])]
toMapping cmd args pkgs = do
  res <- liftIO $ readProcessWithExitCode cmd args []
  case res of 
    (ExitFailure _, out, err') ->
      err $ 
        "failed to retrieve package db's from " ++ cmd ++", output:"
        ++ out ++ '\n':err' 
    (ExitSuccess, out, _) ->
      accumPaths (L.lines out) pkgs []

-- | This returns a non-empty list of package db's, or failure.
fromProvider :: DbProvider -> S.Set String -> M [(FilePath, [(String, Ghc.PackageId)])]
fromProvider prov pkgs = do
  case prov of 
    (Ghc _)          -> toMapping cmd extra_args pkgs 
    (CabalSandbox _) -> toMapping cmd extra_args pkgs
    (Db fp)          -> fromProvider (Ghc . Just $ "--package-db=" ++ fp) pkgs
  where
    (cmd , extra_args) = toExec prov

isConf :: Ghc.PackageId -> P.FilePath -> Bool 
isConf p f = P.hasExtension f "conf" && pkgRelated p f
  
findConf :: Ghc.PackageId -> State (S.Set P.FilePath) (Either Ghc.PackageId P.FilePath)
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
fromPair :: FilePath -> [Ghc.PackageId] -> M [FilePath]
fromPair _ []      =
  return [] 
fromPair db members = do 
  confs <- S.fromList <$> liftIO (listDirectory (P.decodeString db))
  let (remainder, confs') = partitionEithers $ evalState (mapM findConf members) confs 
  unless (L.null remainder) . warning $ 
    "The following packages were not found in the pkg db dir: \n" ++  
    L.intercalate "\n" (map Ghc.packageIdString remainder)
  mapM (return . P.encodeString) confs'

pkgRelated :: Ghc.PackageId -> P.FilePath -> Bool
pkgRelated p = 
  T.isPrefixOf (T.pack . Ghc.packageIdString $ p) 
    . T.pack 
    . P.encodeString 
    . P.filename

pipe_ConfFp :: DbProvider -> PipeM (S.Set String) FilePath ()
pipe_ConfFp prov = do 
  packages <- await 
  if S.null packages 
    then  
      lift . err $ "no results possible due to no packages provided"  
    else do
      lift $ do
        msg "db provider:"
        indentM 2 $ msg . show $ prov 
        msg "\n"
      pairings <- lift $ fromProvider prov packages

      let found = S.unions $ map (S.fromList . map fst . snd) pairings 
          unfound = S.difference packages found 

      if not . S.null $ unfound then
        lift . err $ 
          L.intercalate "\n"  
            ("The following packages were not found in searched package db's:" 
            : S.toList unfound ++ ["Please be sure to provide exact package versions."])

      else -- yield over each returned file,
           --  types are added to make this _much_ easier to understand  
        let 
          mapped :: (FilePath, [(String, Ghc.PackageId)]) -> M [FilePath]
          mapped = uncurry fromPair . (\(db, members) -> (db, map snd members))
        in 
          mapM_ yield =<< lift (L.concat <$> mapM mapped pairings)
