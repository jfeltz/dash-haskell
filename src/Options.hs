{-# LANGUAGE TupleSections #-}

module Options where
import           Pipes

import           Control.Applicative
import           Control.Monad.M

import           Data.Monoid
import qualified Distribution.Package as C
import           Distribution.Text
import qualified Distribution.Version as CV

import           Options.Applicative.Types (readerAsk)
import           Options.Applicative.Builder
import           Options.Applicative.Common
import           Options.Cabal
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
  global           :: Bool,
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
  switch (long "global" <> short 'g' <> help "source packages from global db")
  <*>
  switch (long "user" <> short 'u' <> help "source packages from user db")
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
      <> metavar "ordering=global,user,sandbox .."
      <> help "set ordering")

versionless :: String -> C.PackageId
versionless n = C.PackageIdentifier (C.PackageName n) $ CV.Version [] [] 

-- | This yields requested packages from command line and cabal file, if any.
-- post-condition:
--  a version overlap doesn't exist in dependency list
prod_Dependencies :: Options -> ProducerM [C.Dependency] () 
prod_Dependencies options = do 
  cabal_deps <- lift cabalDeps
  yield . nub' $ cabal_deps ++ packages options
  where
    -- This produces a version disjoint package list from the cabal file.
    cabalDeps :: M [C.Dependency] 
    cabalDeps =  
      maybe 
        (return [])
        (`readPackages` cabalConstraints options) $ cabalFile options
