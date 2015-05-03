-- TODO/FIXME test this step-wite on small package cases, also
-- run diagnistics to otherwise test

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
   
module Options.Cabal where
import qualified Data.Set as S
import           Control.Monad.M


import qualified Data.Map as M
import qualified Data.List as L
import           Control.Monad
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.Package as C
import qualified Distribution.Version as C
import qualified Distribution.PackageDescription as C
import           Data.Either
import           Data.String.Util
import           Control.Monad.IO.Class
import qualified Options.CabalConstraints as OC
import Data.Function (on)

-- | A mapping of cabal build target -> [C.Dependency],
--  e.g. executable:foo to list of cabal dependencies. 
-- Inv: C.Dependencies are non-duplicate
newtype Targets = Targets { mapping :: M.Map String [C.Dependency] }

-- | This is a helper to make the below cleaner.
-- Horribly inefficient, though N of N^2 will be small in practice. 
nubconcat :: (Eq a) => [[a]] -> [a]
nubconcat = L.nub . L.concat   

-- | Return the packages to use from the target type, or fail with
-- unfound targets.
fromTargetType :: S.Set String -> Targets -> Either [String] [C.Dependency] 
fromTargetType narrowing tgts = do
  used_targets <-
    -- Return a list of used targets 
    if S.null narrowing then
       return . M.keysSet . mapping $ tgts 
    else -- Error check narrowed packages
      let found   = S.intersection narrowing $ M.keysSet (mapping tgts)
          unfound = S.difference narrowing found
      in if not . S.null $ unfound then
        Left . S.toList $ unfound 
      else 
        return found 
  
  -- subset of map intersecting with used targets -> reduced list of dependencies
  return $ 
    nubconcat
    . map snd
    . M.toList
    . M.intersection (mapping tgts)
    . M.fromList $ zip (S.toList used_targets) (replicate (S.size used_targets) ())

-- TODO Could be made more resilent WRT changes in cabal file format.
data DependencyDescription =
  DependencyDescription { 
    -- Inv: C.Dependencies are non-duplicate
    library :: Maybe [C.Dependency],
    execs :: Targets,
    suites :: Targets, 
    benchmarks :: Targets
  } 

fromTargets :: Targets -> [C.Dependency] 
fromTargets = nubconcat . M.elems . mapping 
    
toPkgName :: C.Dependency -> String
toPkgName (C.Dependency (C.PackageName name) _) = name
    
-- TODO Test for expected behavior
vintersection :: C.Dependency -> C.Dependency -> Bool
vintersection (C.Dependency _ lv) (C.Dependency _ rv) = 
 C.intersectVersionRanges lv rv == C.noVersion

-- | Post-condition: no version overlap
nub' :: [C.Dependency] -> [C.Dependency]
nub' = L.nubBy (\l r -> toPkgName l == toPkgName r && vintersection l r)
     
fromCabalFile :: 
  FilePath -> DependencyDescription -> OC.CabalConstraints -> M [C.Dependency] 
fromCabalFile cabal desc constraints = 
  case toPkgs pairings of
    Left unfound ->
      err $ preposition "failed to find targets" "in" "cabal file" cabal unfound
    Right found  -> 
      let 
        matched_excluded = 
          S.intersection 
            (S.fromList $ map toPkgName found) (OC.excluded constraints)
      in do
        let
          unfound_excluded =
            S.difference (OC.excluded constraints) matched_excluded 
        -- Print packages which were intended to be excluded, but
        -- weren't found anyway.  
        unless (S.null unfound_excluded) $
          warningList "packages to exclude were not found:" . S.toList $
           unfound_excluded 
         
        -- Calculate the packages with overlapped ranges 
        let 
            unexcluded = 
              L.filter (flip S.member matched_excluded . toPkgName) found
            sorted     = name_sorted unexcluded -- sorted for readability
            -- nub' can be pretty destructive here if there is range overlap
            -- in the versions. dash-haskell can't be expected to otherwise
            -- determine the user's intent in this situation, so we instead
            -- remove offenders.
            disjoint   = nub' sorted
            overlapped = sorted L.\\ disjoint 
        
        -- Print version overlapped (removed packages) if any.  
        unless (L.null overlapped) $
          warningList 
            ("removed the following packages from processing due version" 
            ++ " range overlap:")
            overlapped

        return disjoint 
  where
    name_sorted :: [C.Dependency] -> [C.Dependency] 
    name_sorted = L.sortBy (on compare toPkgName) 

    -- | Produce a list of targets to evaluate based off selection, i.e.
    -- if any fst member of tuple is non-empty, the subset is returned.
    -- if all are non-empty, all are considered 
    
    toPkgs :: 
      [(S.Set String, Targets)] -- Filtered packages for targets 
      -> Either [String] [C.Dependency] 
    toPkgs list =
      nubconcat <$> case L.partition (S.null . fst) list of
        (non_selections, []) -> 
          Right . map (fromTargets . snd) $ non_selections  
        (_, selections)      -> 
          case partitionEithers (L.map (uncurry fromTargetType) selections) of 
            ([],lists)   -> Right lists 
            (unfound,_) -> Left . nubconcat $ unfound 
      
    -- | Return pairings of expected cabal targets to actual cabal targets
    pairings :: [(S.Set String, Targets)] 
    pairings = 
      let 
        lib_pairing :: (S.Set String, Targets)
        lib_pairing = 
          (if OC.lib constraints then S.singleton "library" else mempty,
           Targets $ maybe mempty (M.singleton "library") (library desc)) 
      in
      lib_pairing :
        zip (map ($ constraints) [OC.execs, OC.suites, OC.benchmarks])
            (map ($ desc)   [execs, suites, benchmarks]) 

    -- | Filter the dependency list by set name membership,
    set_intersection :: S.Set String -> [C.Dependency] -> [C.Dependency] 
    set_intersection selected_packages = 
      L.filter (\d -> S.member (toPkgName d) selected_packages) 
    
toStrName :: C.Dependency -> String
toStrName (C.Dependency (C.PackageName name) _) = name
  
toTargets :: [(String, C.CondTree a [C.Dependency] b)] -> Targets
toTargets = 
    Targets . M.fromList . map (uncurry toDeps)
  where 
    toDeps target tree = (target, C.condTreeConstraints tree)

-- | Given the defined constraints, return a list with the
-- following properties: 
-- 1 version overlap is not a relation for deps's taken as a set 
-- 2 unversioned packageId's satisfy cabal constraints 
readPackages :: FilePath -> OC.CabalConstraints -> M [C.Dependency]
readPackages cabal_path constraints = do 
  parse_result <- liftIO $ C.parsePackageDescription <$> readFile cabal_path
  case parse_result of
    (C.ParseFailed fail_msg) ->
      err . show $ fail_msg
    (C.ParseOk warnings desc) -> do
      unless (L.null warnings) . warning $ 
        preposition 
          "warnings during parse" 
          "of"
          "cabal file"
          "warnings"
          (map show warnings)
      fromCabalFile cabal_path (toDescription desc) constraints 
   where
    -- Produce a simplified description of the cabal file for processing.
    toDescription :: C.GenericPackageDescription -> DependencyDescription 
    toDescription gpd =  
      DependencyDescription
        (C.condTreeConstraints <$> C.condLibrary gpd)
        (toTargets . C.condExecutables $ gpd)
        (toTargets . C.condTestSuites $ gpd)
        (toTargets . C.condBenchmarks $ gpd)
