module Db where
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.M
import           FilePath
-- import qualified Data.List as L
import           Text.ParserCombinators.Parsec
-- import qualified Data.Set as S

-- import qualified Distribution.Package             as CP
-- import qualified Distribution.ParseUtils          as CP
import qualified Distribution.Simple.Compiler        as CC
import qualified Distribution.Simple.GHC             as CG
-- import qualified Distribution.Simple.Configure       as CC
import qualified Distribution.Simple.PackageIndex    as CI

import qualified Distribution.Simple.Program.Builtin as CP
import qualified Distribution.Simple.Program.Db      as CP
import qualified Distribution.Simple.Program         as CP

import qualified Distribution.Verbosity           as CVB
import qualified Distribution.Version             as CV
import qualified Distribution.Text                as CT

import           Options.Applicative.Types (ReadM, readerError)

-- data GhcPkg = GhcPkg { args :: [String] , stack :: Db }
-- data Db = Single FilePath | Ghc (Maybe String) 
   
data Db = 
  -- A cabal sandbox db, with optional path to sandbox configuration file  
 Sandbox FilePath
  -- The global ghc package database
 | Global                 
  -- The user package database, as defined by cabal 
 | User                   
  -- A direct database path
 | Path FilePath deriving (Ord, Eq)
 
instance (Show Db) where
  show (Sandbox p) = "cabal sandbox db with config file: " ++ p 
  show (Global   ) = "ghc distribution (global) db" 
  show (User     ) = "system user db" 
  show (Path    p) = "db path: " ++ p

fromSplit :: Char -> String -> ReadM (String, Maybe String)
fromSplit c opt = 
  case opt of 
    []      -> return ([], Nothing) 
    (s:str) ->
     if s == c then do
       param <- fromParam str 
       return ([], param)
     else do 
       (l, r) <- fromSplit c str
       return (s:l, r)
  where 
    fromParam []      =  return Nothing
    fromParam (c':str)=
      if c' == c then
        readerError $ "encountered delimeter(" ++ c:") twice"
      else
        Just . maybe [c'] (c':) <$> fromParam str

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

fromStack :: [CC.PackageDB] -> M CI.InstalledPackageIndex 
fromStack stack = do 
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

