-- TODO ensure this handles hidden cases
{-# LANGUAGE OverloadedStrings #-}
module Pipe.Db where
import           Pipes
import           Control.Monad
import           Control.Monad.M
-- import           Control.Monad.State
-- import           Data.Either
-- import qualified Data.List as L
-- import qualified Data.Set as S
-- import qualified Data.Text as T
-- import           System.FilePath hiding (readFile)
-- import qualified System.FilePath as P
import           Db
import           FilePath
import           Text.ParserCombinators.Parsec hiding (State)
import qualified Distribution.InstalledPackageInfo   as CI
import qualified Distribution.Simple.Compiler        as CC
import qualified Distribution.Simple.GHC             as CG
import qualified Distribution.Simple.PackageIndex    as CI
import qualified Distribution.Simple.Program.Builtin as CP
import qualified Distribution.Simple.Program.Db      as CP
import qualified Distribution.Simple.Program         as CP
import qualified Distribution.Package as C
import qualified Distribution.Verbosity           as CVB
import qualified Distribution.Version             as CV
import qualified Distribution.Text        as CT
import qualified Module as Ghc
import Data.Maybe
import PackageConf

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
  fromE $ runParser (singleField "package-db") () path buf

cabalDb :: Db -> M CC.PackageDB
cabalDb (Sandbox config) = do
  check_result <- liftIO $ checkFile config "cabal sandbox config"
  case check_result of
    Nothing      -> CC.SpecificPackageDB <$> parsedDbPath config 
    Just err_str -> err err_str 
cabalDb (Global   ) = return CC.GlobalPackageDB
cabalDb (User     ) = return CC.UserPackageDB
cabalDb (Path    p) = return . CC.SpecificPackageDB $ p
        
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

fromIndex :: C.Dependency -> CI.InstalledPackageIndex -> Maybe Conf 
fromIndex dep index = 
  let 
    -- For clarity:
    versions :: [(CV.Version, [CI.InstalledPackageInfo])]
    versions = CI.lookupDependency index dep
  in
    listToMaybe . catMaybes . concat . map (map toConf . snd) $ versions 
  where
    toConf :: CI.InstalledPackageInfo -> Maybe Conf 
    toConf info = do 
      interfaceFile' <- listToMaybe $ CI.haddockInterfaces info
      htmlDir'       <- listToMaybe $ CI.haddockHTMLs info
      return $ 
        Conf 
          (Ghc.stringToPackageKey . show . CT.disp $ CI.sourcePackageId info)
          interfaceFile' htmlDir' 
          (CI.exposed info)
