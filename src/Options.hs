{-# LANGUAGE TupleSections #-}

module Options where

import           Control.Applicative
import           Control.Monad.M
import qualified Data.List as L

import           Data.Monoid
import qualified Distribution.Package as C
import           Distribution.Text
import qualified Distribution.Version as CV

import           Options.Applicative.Types (readerAsk)
import           Options.Applicative.Builder
import           Options.Applicative.Common
import           Options.Cabal
import           Options.CabalConstraints (toConstraints, none, CabalConstraints)
import           Options.DbProvider

import           PackageId
import           Pipes

data Options = Options { 
  dbprovider :: DbProvider,
  outputDir  :: FilePath,
  quiet    :: Bool,
  cabalFile :: Maybe FilePath,
  cabalConstraints :: CabalConstraints, 
  packages :: [C.Dependency]
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
parser = 
  Options <$> 
    option toProvider 
      (long "dbprovider" 
      <> short 'p'
      <> metavar "<provider,args>"
      <> value (CabalSandbox Nothing) 
      <> help "a ghc package db provider: cabal|ghc|dir\n")
    <*>
    strOption (
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
     help "a list of packages to specifically build, e.g. either-1.0.1 text"
     ))

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
