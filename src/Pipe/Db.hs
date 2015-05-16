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
import           Filesystem hiding (readFile)
import qualified Filesystem.Path.CurrentOS as P
import           Db
-- imports necessary for working with Cabal package db tools 
import Distribution.Package
-- import           Control.Monad.IO.Class
import           FilePath
import           Text.ParserCombinators.Parsec hiding (State)
import qualified Distribution.Simple.Compiler        as CC
import qualified Distribution.Simple.GHC             as CG
import qualified Distribution.Simple.PackageIndex    as CI
import qualified Distribution.Simple.Program.Builtin as CP
import qualified Distribution.Simple.Program.Db      as CP
import qualified Distribution.Simple.Program         as CP
import qualified Distribution.Verbosity           as CVB
import qualified Distribution.Version             as CV
import qualified Distribution.Text                as CT

type DbStack = [Db]

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
      
      -- pairings <- lift $ fromProvider prov dependencies
      let pairings = []

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

field :: String -> Parser String
field str =
  string str 
  >> char ':'
  >> many (char ' ') 
  >> manyTill anyToken (void (char '\n') <|> eof)

singleField :: String -> Parser String 
singleField str = try (field str) <|> (anyToken >> singleField str) 
 
parsedDbPath :: String -> M String 
parsedDbPath path = do
  buf <- liftIO $ readFile path
  case runParser (singleField "package-db") () path buf of
    Left  str -> err . show $ str
    Right db      -> return db 

cabalDb :: Db -> M CC.PackageDB
cabalDb (Sandbox config) = do
  check_result <- liftIO $ checkFile config "cabal sandbox config"
  case check_result of
    Nothing      -> CC.SpecificPackageDB <$> parsedDbPath config 
    Just err_str -> err err_str 
cabalDb (Global   ) = return CC.GlobalPackageDB
cabalDb (User     ) = return CC.UserPackageDB
cabalDb (Path    p) = return . CC.SpecificPackageDB $ p
        
-- | This will warn here or downstream when using out of bounds haddock 
-- ghc-pkg: >=7.10 && <7.10.2
ghcVersionRange :: CV.VersionRange
ghcVersionRange = 
 CV.intersectVersionRanges 
   (CV.orLaterVersion (CV.Version [7,10]   [])) 
   (CV.earlierVersion (CV.Version [7,10,2] [])) 

toIndex :: [CC.PackageDB] -> M CI.InstalledPackageIndex 
toIndex stack = do 
  version <- liftIO $ CP.programFindVersion CP.ghcPkgProgram CVB.normal "ghc-pkg"
  case version of
    Nothing ->
      warning $ "unable to determine ghc-pkg version, \n" ++ clause
    Just v ->
      unless (not $ CV.withinRange v ghcVersionRange) $ 
        warning $
        "ghc-pkg version: " ++ show version ++ "not within allowable range, \n" 
        ++ clause
  
  liftIO $ CG.getInstalledPackages CVB.normal stack program_conf
  where
    program_conf :: CP.ProgramConfiguration
    program_conf = CP.restoreProgramDb [CP.ghcPkgProgram] CP.emptyProgramDb
    clause :: String
    clause = 
      "results may not match current supported haddock: " 
      ++ show (CT.disp ghcVersionRange) 

-- TODO 
-- apply lookupDependency :: PackageInstalled a => PackageIndex a -> Dependency -> [(Version, [a])] 
-- at this point
