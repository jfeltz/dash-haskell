module Pipe.Conf where
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
import Data.String.Util

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

cabalSandboxConfig :: FilePath
cabalSandboxConfig = "./cabal.sandbox.config"

cabalDb :: Db -> M CC.PackageDB
cabalDb db =
  case db of
    (Global ) -> return CC.GlobalPackageDB
    (User   ) -> return CC.UserPackageDB
    (Dir  p ) -> do
      fromCheck p =<< liftIO (checkDir p "package-db")
    s@(Sandbox) -> do
      result <- liftIO $ checkFile cabalSandboxConfig (show s)
      case result of
        Nothing -> do
          db'    <- parsedDbPath cabalSandboxConfig
          fromCheck db' =<< liftIO (checkDir db' "sandbox package-db")
        Just error_str ->
          err error_str
   where
     fromCheck :: FilePath -> Maybe String -> M CC.PackageDB
     fromCheck path Nothing          = return $ CC.SpecificPackageDB path
     fromCheck _    (Just error_str) = err error_str

ghcVersionRange :: CV.VersionRange
ghcVersionRange =
 CV.intersectVersionRanges
   (CV.orLaterVersion (CV.Version [7,10]   []))
   (CV.earlierVersion (CV.Version [8,0,2,1] []))

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
    (compiler, _, conf) <- CG.configure CVB.normal Nothing Nothing CP.defaultProgramConfiguration

    CG.getInstalledPackages CVB.silent compiler stack conf
  where
    clause :: String
    clause =
      "results may not match current supported haddock: "
      ++ show (CT.disp ghcVersionRange)

fromIndex :: C.Dependency -> CI.InstalledPackageIndex -> Maybe PackageConf
fromIndex dep index =
  let
    -- For clarity:
    versions :: [(CV.Version, [CI.InstalledPackageInfo])]
    versions = CI.lookupDependency index dep
  in
    listToMaybe . catMaybes . concatMap (map toConf . snd) $ versions
  where
    toConf :: CI.InstalledPackageInfo -> Maybe PackageConf
    toConf info = do
      interfaceFile' <- listToMaybe $ CI.haddockInterfaces info
      htmlDir'       <- listToMaybe $ CI.haddockHTMLs info
      return $
        PackageConf
          (Ghc.stringToUnitId . show . CT.disp $ CI.sourcePackageId info)
          interfaceFile' htmlDir'
          (CI.exposed info)

toOptionDbs :: O.Options -> S.Set Db
toOptionDbs options =
  S.fromList . catMaybes $
    (Dir <$> O.db options)
    : map (uncurry toMaybe)
        [ (not $ O.nouser options, User), (O.sandbox options, Sandbox) ]

-- | Produce remaining db's given total ordering of dbs and possibly
-- smaller list of dbs.
fromOrdering :: [Db] -> S.Set Db -> Either String [Db]
fromOrdering []           s =
  if S.null s then
    Right []
  else
    Left $ "failed to match package db(s) with defined ordering, dbs:\n" ++
           (indenting 2 . listing $ S.toList s)
fromOrdering (o:ordering) s =
  if not $ S.member o s then
    fromOrdering ordering s
  else
    case L.find (o ==) (S.elems s) of
       Nothing -> Left "failed to find matched db in ordering"
       Just e  -> (e :) <$> fromOrdering ordering (S.delete o s)

pipe_PackageConf :: O.Options -> PipeM C.Dependency PackageConf ()
pipe_PackageConf options = do
  index <- lift $ do
    dbs <- -- Right now the cabal API requires a global db,
          -- and for that to be first.
      (Global:)
      <$>
      fromE (fromOrdering (O.dbOrdering options) $ toOptionDbs options)
    liftIO . putStr $
      "using package db stack:\n"
      ++ (listing $ map ((++) " > " . show) dbs)
      ++ "\n\n"
    toIndex =<< stackreverse <$> mapM cabalDb dbs
  forever $ do
    dep <- await
    case fromIndex dep index of
      Nothing ->
        lift . warning $
          "failed to find suitable documentation candidate for package:\n "
          ++ (show . CT.disp $ dep)
      Just conf -> do
        strings <- liftIO $ problems conf
        if L.null strings then
          yield conf
        else do
          lift . warning $
            "skipping package: " ++ show (CT.disp dep) ++ ", with conf problems:\n"
             ++ indenting 2 (listing strings)
          return ()
  where
    -- | Counter-intuitively,
    -- Cabal right now actually evaluates the stack from right to left.
    -- not left to right.. and we still have to preserve the first member
    -- (Global)
    stackreverse s = head s : L.reverse (drop 1 s)
