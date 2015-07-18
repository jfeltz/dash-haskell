{-# LANGUAGE FlexibleContexts #-}
module Options where
import           Data.Monoid
import qualified Distribution.Package as C
import           Distribution.Text
import qualified Distribution.Version as V

import           Options.Applicative.Types (readerAsk, fromM, manyM)
import           Options.Applicative.Builder
import           Options.Applicative.Common
import qualified Text.ParserCombinators.Parsec as P
import           Db
import           Options.Db
import qualified Data.List as L

data Options = Options { 
  outputDir        :: FilePath,
  quiet            :: Bool,
  cabalFile        :: Maybe FilePath,
  cabalExclusions  :: [String], 
  sandbox          :: Bool, 
  nouser           :: Bool,
  db               :: Maybe FilePath,
  dbOrdering       :: [Db], 
  packages         :: [C.Dependency]
} deriving Show

parser :: Parser Options
parser =
  Options <$> 
  (strOption $ 
    long "output" 
    <> short 'o'
    <> metavar "<dir>" 
    <> value "./docsets" 
    <> help "the directory to write created docsets to")
  <*>
  switch (long "quiet" <> short 'q' <> help "set to quiet output")
  <*>
  option (Just <$> readerAsk)
    (long "cabal"
    <> short 'c'  
    <> metavar "<file.cabal>" 
    <> value Nothing
    <> help "the cabal file to retrieve package dependencies from")
  <*>
  option toPackageL
    (long "cabal-excludes"
      <> short 'x'
      <> value []
      <> metavar "ghc,lens.."
      <> help "limit package results from a cabal file source")
  <*>
  switch (long "sandbox" <> short 's' <> help "use cabal sandbox")
  <*>
  switch (long "no-user" <> short 'n' <> help "don't source packages from user db")
  <*>
  option fp
    (long "db"
    <> metavar "<path-to-package-db>"
    <> value Nothing
    <> help "package db directory")
  <*>
  option toOrdering
    (long "ordering"
      <> short    'd'
      <> value    defaultOrdering 
      <> metavar "user,sandbox.."
      <> help    "ordering of user, dir, and sandbox db's")
  <*>
  (fromM . manyM $ (
    argument dep 
    (metavar "packages" <>
     help "a list of packages to specifically build, e.g. either-1.0.1 text")))
  where
    fp :: ReadM (Maybe FilePath)
    fp = do
      str' <- str  
      return $ if (L.null str') then Nothing else (Just str')

    versionless :: C.PackageIdentifier -> Bool 
    versionless p = C.packageVersion p == V.Version [] []

    dep :: ReadM C.Dependency 
    dep = do
      pkg_str <- readerAsk
      case simpleParse pkg_str of 
        Nothing                  ->
          readerError $ "failed parsing package: "
        Just pkg ->
          return $ 
            if versionless pkg then
              C.Dependency (C.packageName pkg) V.anyVersion 
            else
              C.thisPackageVersion pkg

toPackageL :: ReadM [String]
toPackageL = do
    expr <- readerAsk
    case P.parse parser' [] expr of 
      Left err -> readerError . show $ err
      Right pkgs -> return pkgs 
    where
      parser' :: P.Parser [String]
      parser' =
        P.sepBy
          (P.many1 (P.notFollowedBy (P.char ',') >> P.letter))
          (P.char ',') 

