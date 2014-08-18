{-# OPTIONS_GHC -fno-warn-orphans #-}

module Options where

import Options.Applicative.Common
import Options.Applicative.Builder
import Options.Cabal
import Options.DbProvider
import Options.CabalConstraints (toConstraints, none, CabalConstraints)
import Control.Applicative
import Data.Char
import qualified Data.List as L
import Package (unversioned)
import Control.Monad.M
import Data.Monoid
import Pipes
import qualified Data.Set as S

data Options = Options { 
  dbprovider :: DbProvider,
  output_dir  :: FilePath,
  quiet    :: Bool,
  cabal :: Maybe FilePath,
  cabalConstraints :: CabalConstraints, 
  packages :: [Package]
} deriving Show

parsePkg :: String -> Maybe Package
parsePkg s = 
  if all (\c -> isAlphaNum c || (c `elem` "-.")) s then 
    Just s
  else
    Nothing 

parser :: Parser Options
parser = 
  Options <$> 
    nullOption (
      eitherReader toProvider
      <> long "dbprovider" 
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
    switch (long "quiet" <> short 'q' <> help "set to verbose output")
    <*>
    nullOption (
     eitherReader (Right . Just)
     <> long "cabal"
     <> short 'c'  
     <> metavar "<file.cabal>" 
     <> value Nothing
     <> help "the cabal file to source package dependencies from")
    <*>
    nullOption (
      eitherReader toConstraints
      <> long "cabal-constraints" 
      <> short 'r'
      <> value none 
      <> metavar "executable=name, .."
      <> help "limit package results from a cabal file source, see documentation")
    <*>
    many (
     argument parsePkg (metavar "packages" <> 
     help "a list of packages to specifically build, e.g. either-1.0.1 text"
     ))

prod_Packages :: Options -> ProducerM (S.Set Package) () 
prod_Packages options = do
  cabal_packages <-
    lift $ case cabal options of 
      Nothing -> return mempty 
      Just fp -> readPackages fp (cabalConstraints options)
  yield . reduce (packages options) $ S.toList cabal_packages
  where 
    -- | Given two lists of package satisfying strings, 
    -- return a list that non-duplicate, the most versioned of the two.
    -- e.g. if 'either-4.1.0' is in one, and 'either' is the other, 
    -- the versioned is chosen. If both are versioned, both appear in
    -- the final result.
    reduce :: [Package] -> [Package] -> S.Set Package
    reduce l r = 
      S.fromList . fromAsc $ L.sort $ l ++ r
     where
      -- Here we exploit the fact that you only need to examine the next member of 
      -- an ascending list to determine a version
      fromAsc :: [String] -> [String]
      fromAsc []      = [] 
      fromAsc (p:[])  = [p] 
      fromAsc (p:nxt:rest)
        | p == nxt = -- duplicate 
          fromAsc (nxt:rest) 
        | unversioned p == unversioned nxt = 
          if unversioned p == p then
            fromAsc $ nxt:rest
          else -- both are versioned by list ordering
            p : nxt : fromAsc rest 
        | otherwise = -- they're different packages
            p : fromAsc ( nxt : rest )
