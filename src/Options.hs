{-# LANGUAGE TupleSections #-}

module Options where

import           Control.Applicative
import           Control.Monad.M
import qualified Data.List as L

import           Data.Monoid
import qualified Distribution.Package as C
import qualified Distribution.Version as CV
import           Distribution.Text
import           Options.Applicative.Builder
import           Options.Applicative.Common
import           Options.Cabal
import           Options.CabalConstraints (toConstraints, none, CabalConstraints)
import           Options.DbProvider
import           PackageId
import           Pipes
import qualified Data.Set as S
                 
data Options = Options { 
  dbprovider :: DbProvider,
  outputDir  :: FilePath,
  quiet    :: Bool,
  cabal :: Maybe FilePath,
  cabalConstraints :: CabalConstraints, 
  packages :: [C.PackageId]
} deriving Show

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
    option (return . Just) 
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
     argument simpleParse (metavar "packages" <> 
     help "a list of packages to specifically build, e.g. either-1.0.1 text"
     ))

-- | Given two lists of package satisfying strings, 
-- return a list that is non-duplicate, the most versioned of the two.
-- e.g. if 'either-4.1.0' is in one, and 'either' is the other, 
-- the versioned is chosen. If both are versioned, both appear in
-- the final result.
reduce :: [C.PackageId] -> [C.PackageId]
reduce = fromAsc . L.sort where
  -- Here we exploit the fact that you only need to examine the next member of 
  -- an ascending list to determine a version
  fromAsc :: [C.PackageId] -> [C.PackageId]
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
  
versionless :: String -> C.PackageId
versionless n = C.PackageIdentifier (C.PackageName n) $ CV.Version [] [] 

prod_Packages :: Options -> ProducerM (S.Set String) () 
prod_Packages options = do
  cabal_dependencies <-
    lift $ case cabal options of 
      Nothing -> return [] 
      Just fp -> (S.toList . S.map versionless) 
                 <$> readPackages fp (cabalConstraints options)
  yield . S.fromList . map (show . disp) $ 
    reduce (packages options ++ cabal_dependencies)
