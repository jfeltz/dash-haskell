module Options where
import           Control.Applicative
import           Data.Monoid
import qualified Distribution.Package as C
import           Distribution.Text

import           Options.Applicative.Types (readerAsk)
import           Options.Applicative.Builder
import           Options.Applicative.Common
import           Options.CabalConstraints (toConstraints, none, CabalConstraints)
import           Db
import           Options.Db

data Options = Options { 
  outputDir        :: FilePath,
  quiet            :: Bool,
  cabalFile        :: Maybe FilePath,
  cabalConstraints :: CabalConstraints, 
  packages         :: [C.Dependency],
  sandbox          :: Maybe (Maybe FilePath),
  user             :: Bool,
  db               :: Maybe FilePath,
  dbOrdering       :: [Db] 
} deriving Show

packageReadM :: Text a => ReadM a
packageReadM = do
  s <- readerAsk
  maybe 
   (readerError $ "failed parsing packages:\n " ++ s)
   return 
   -- Qualified, as simpleParse is a little obscure.
   (Distribution.Text.simpleParse s) 

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
  many (
    argument packageReadM (metavar "packages" <>
    help "a list of packages to specifically build, e.g. either-1.0.1 text"))
  <*>
  option 
    (Just . Just <$> readerAsk)
    (long "sandbox"
      <> short 's' 
      <> metavar "<configuration-file-path>"
      <> value Nothing
      <> help "package sandbox file")
  <*>
  switch (long "no-user" <> short 'u' <> help "don't source packages from user db")
  <*>
  option 
    (Just <$> readerAsk)
    (long "db" 
      <> metavar "<path-to-package-db>"
      <> value Nothing
      <> help "package db directory")
  <*>
  option toOrdering
    (long "ordering"
      <> short 'o'
      <> value dbPaths 
      <> metavar "ordering=user,sandbox .."
      <> help "set ordering")
