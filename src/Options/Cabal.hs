{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
   
module Options.Cabal where
import qualified Data.Set as S
import           Control.Monad.M

import qualified Data.List as L
import           Control.Monad
import qualified Distribution.PackageDescription.Parsec as C
import qualified Distribution.Package as C
import qualified Distribution.Version as C
import qualified Distribution.PackageDescription as C
import           Distribution.Verbosity
import           Data.String.Util
import           Control.Monad.IO.Class
import           Data.Function (on)
import           Data.Maybe

toPkgName :: C.Dependency -> String
toPkgName (C.Dependency name _) = C.unPackageName name

vintersection :: C.Dependency -> C.Dependency -> Bool
vintersection (C.Dependency _ lv) (C.Dependency _ rv) = 
 not $ C.intersectVersionRanges lv rv == C.noVersion

-- | Post-condition: no version overlap
nub' :: [C.Dependency] -> [C.Dependency]
nub' = L.nubBy (\l r -> toPkgName l == toPkgName r && vintersection l r)

fromExclusions ::  S.Set String -> [C.Dependency] -> M [C.Dependency] 
fromExclusions exclusions deps = do 
  -- Print packages which were intended to be excluded, but
  -- weren't found anyway.  
  unless (S.null unfound_exclusions) $
    warning
      . listing
      . ("packages to exclude were not found:":)
      . map toString
      . S.toList
      $ excluded 
  -- Print version overlapped (removed packages) if any.
  unless (L.null overlapped) $
    warning $ 
      ("removed the following packages from processing due version" 
      ++ " range overlap:\n") ++ (indenting 2 . listing $ overlapped)
  return disjoint 
         
  where
    excluded = S.intersection (S.fromList $ map toPkgName deps) exclusions
    unfound_exclusions = S.difference exclusions excluded
    unexcluded = L.filter (not . flip S.member excluded . toPkgName) deps
    sorted     = L.sortBy (on compare toPkgName) unexcluded -- for readability
    disjoint   = nub' sorted
    -- | Calculate the packages with overlapped ranges 
    overlapped = sorted L.\\ disjoint 

-- | Given the defined exclusion set, return a list with the
-- following properties: 
-- 1 version overlap is not a relation for deps's taken as a set 
-- 2 unversioned packageId's satisfy cabal constraints 
readPackages :: FilePath -> S.Set String -> M [C.Dependency]
readPackages cabal_path exclusions = do 
  parse_result <- liftIO $ C.readGenericPackageDescription normal cabal_path
  fromExclusions exclusions . toDeps $ parse_result
   where
    toDeps :: C.GenericPackageDescription -> [C.Dependency] 
    toDeps gpd =
      concatMap ($ gpd) [
        concatDeps . maybeToList . C.condLibrary,
        concatDeps . map snd . C.condExecutables,
        concatDeps . map snd . C.condTestSuites,
        concatDeps . map snd . C.condBenchmarks 
        ]
      where
        concatDeps :: [C.CondTree v [C.Dependency] a] -> [C.Dependency]
        concatDeps = concatMap C.condTreeConstraints
