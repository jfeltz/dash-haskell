module Pipe.Conf where
-- import Data.String.Util
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.M
import PackageConf
import FilePath       
import Text.ParserCombinators.Parsec hiding (State)
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
import Db
import qualified Options as O
import           Data.Maybe
import           Data.Maybe.Util
import qualified Data.List as L
import qualified Data.Set  as S
import Pipes
import qualified Module as Ghc

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
      warning $
        "unable to determine ghc-pkg version, \n" ++ clause
    Just v ->
      unless (CV.withinRange v ghcVersionRange) $ 
        warning $
          "ghc-pkg version: "
          ++ show (CT.disp v) ++ " not within allowable range,\n"
          ++ clause
  liftIO $ do
    minimal_programs <- CP.configureAllKnownPrograms CVB.normal $ 
      CP.restoreProgramDb [CP.ghcPkgProgram, CP.ghcProgram] CP.emptyProgramDb

    CG.getInstalledPackages CVB.silent stack minimal_programs
  where
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
    listToMaybe . catMaybes . concatMap (map toConf . snd) $ versions 
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

toOptionDbs :: O.Options -> S.Set Db
toOptionDbs options = 
  S.fromList . catMaybes $ 
    (Path <$> O.db options) 
    : (Sandbox <$> O.sandbox options)
    : [ toMaybe (not $ O.user options) User ]

-- | Produce remaining db's given total ordering of dbs and possibly
-- smaller list of dbs.
fromOrdering :: [Db] -> S.Set Db -> Either String [Db]
fromOrdering []           s =
  if S.null s then
    Right []
  else
    Left "failed to match package db listing with undefined ordering" 
fromOrdering (o:ordering) s =  
  if not $ S.member o s then 
    fromOrdering ordering s
  else
    case L.find (o ==) (S.elems s) of
       Nothing -> Left "failed to find matched db in ordering"
       Just e  -> (e :) <$> fromOrdering ordering (S.delete o s)

pipe_Conf :: O.Options -> PipeM C.Dependency Conf ()
pipe_Conf options = do
  index <- lift $ do 
    dbs <-
      (Global:)
      <$>
      fromE (fromOrdering (O.dbOrdering options) $ toOptionDbs options)
    liftIO . putStr $
      "using package db stack:\n > "
      ++ L.intercalate "\n > " (map show dbs)
      ++ "\n\n"
    toIndex =<< mapM cabalDb dbs
  forever $ do
    dep <- await
    case fromIndex dep index of 
      Nothing -> 
        lift . warning $ 
          "failed to find suitable documentation candidate for package:\n " 
          ++ (show . CT.disp $ dep) 
      Just conf -> 
          yield conf
