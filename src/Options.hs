{-# LANGUAGE FlexibleContexts #-}
module Options where
import           Data.Monoid
import qualified Distribution.Package as C
import           Distribution.Text
import qualified Distribution.Version as V

import           Options.Applicative.Types (readerAsk, fromM, manyM)
import           Options.Applicative.Builder
import           Options.Applicative.Common
import           Options.CabalConstraints (toConstraints, none, CabalConstraints)
import           Db
import           Options.Db
import qualified Data.List as L

-- cabalSandboxConfig :: FilePath
-- cabalSandboxConfig = "./cabal.sandbox.config"       

data Options = Options { 
  outputDir        :: FilePath,
  quiet            :: Bool,
  cabalFile        :: Maybe FilePath,
  cabalConstraints :: CabalConstraints, 
  sandbox          :: Bool, 
  user             :: Bool,
  db               :: Maybe FilePath,
  dbOrdering       :: [Db], 
  packages         :: [C.Dependency]
} deriving Show

parser :: Parser Options
parser = Options <$> 
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
  option toConstraints
    (long "cabal-constraints"
      <> short 'r'
      <> value none 
      <> metavar "executable=name, .."
      <> help "limit package results from a cabal file source, see documentation")
  <*>
  switch (long "sandbox" <> short 's' <> help "use cabal sandbox")
  <*>
  switch (long "no-user" <> short 'u' <> help "don't source packages from user db")
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
      <> metavar "ordering=user,sandbox .."
      <> help    "set ordering")
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
