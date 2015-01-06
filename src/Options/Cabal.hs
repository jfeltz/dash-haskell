{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
   
module Options.Cabal where
import qualified Data.Set as S
import           Control.Monad.M
import           Data.Monoid
import qualified Data.Map as M
import qualified Data.List as L
import           Control.Applicative
import           Control.Monad
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.Package as C
import qualified Distribution.PackageDescription as C
import           Data.Either
import           Data.String.Util
import           Control.Monad.IO.Class
import qualified Options.CabalConstraints as OC

type Package = String
newtype Targets = Targets { mapping :: M.Map String (S.Set Package) }

fromTargets :: Targets -> S.Set Package
fromTargets = S.unions . M.elems . mapping 
    
-- | Return the packages to use from the target type, or fail with unfound targets.
fromTargetType :: S.Set String -> Targets -> Either [String] (S.Set String) 
fromTargetType narrowing tgts = do
  used_targets <- -- return a list of used targets 
    if S.null narrowing then
       return . M.keysSet . mapping $ tgts 
    else -- error check narrowed packages
      let found   = S.intersection narrowing $ M.keysSet (mapping tgts)
          unfound = S.difference narrowing found
      in if not . S.null $ unfound then
        Left . S.toList $ unfound 
      else 
        return found 
  -- accumulate dependencies from found keys
  return $
    S.unions 
    . map snd
    . M.toList
    . M.intersection (mapping tgts)
    . M.fromList $ zip (S.toList used_targets) (replicate (S.size used_targets) ())

data DependencyDescription =
  DependencyDescription { 
    library :: Maybe (S.Set Package),
    execs :: Targets,
    suites :: Targets, 
    benchmarks :: Targets
 } 

toPackages :: FilePath -> DependencyDescription -> OC.CabalConstraints -> M (S.Set Package) 
toPackages cabal desc constraints = 
  case toPkgs pairings of
    Left unfound ->
      err $ 
        preposition 
          "failed to find targets" 
          "in"
          "cabal file"
          cabal 
          unfound
    Right set  ->
      let 
        matched_excluded = S.intersection set (OC.excluded constraints)
        remainder = S.difference (OC.excluded constraints) matched_excluded 
      in do
      unless (S.null remainder) . 
        warning . 
          L.intercalate "\n" $
          "packages to exclude were not found:" : S.toList remainder 
      return $ S.difference set (OC.excluded constraints)
  where
    -- | Produce a list of targets to evaluate based off selection, e.g.
    -- if any fst member of tuple is non-empty, the subset is returned.
    -- if all are non-empty, all are considered 
    toPkgs :: [(S.Set String, Targets)] -> Either [String] (S.Set Package) 
    toPkgs list =
      case L.partition (S.null . fst) list of
        (non_selections, []) -> 
          Right . S.unions . map (fromTargets . snd) $ non_selections  
        (_, selections)      -> 
          case partitionEithers (L.map (uncurry fromTargetType) selections) of 
            ([],sets)   -> Right . S.unions $ sets 
            (unfound,_) -> Left . concat $ unfound 
      
    -- | Return pairings of expected cabal targets to actual cabal targets
    pairings :: [(S.Set String, Targets)] 
    pairings = 
      let 
        lib_pairing :: (S.Set Package, Targets)
        lib_pairing = 
          (if OC.lib constraints then S.singleton "library" else mempty,
           Targets $ maybe mempty (M.singleton "library") (library desc)) 
      in
      lib_pairing :
        zip (map ($ constraints) [OC.execs, OC.suites, OC.benchmarks])
            (map ($ desc)   [execs, suites, benchmarks]) 

toStrName :: C.Dependency -> String
toStrName (C.Dependency (C.PackageName name) _) = name
  
toTargets :: [(String, C.CondTree a [C.Dependency] b)] -> Targets
toTargets = 
  let 
    toStrDeps target tree = (target, S.fromList . map toStrName $ C.condTreeConstraints tree) 
  in
    Targets . M.fromList . map (uncurry toStrDeps)

-- | Given the defined constraints,
-- return packages satisfying from the cabal file.
readPackages :: FilePath -> OC.CabalConstraints -> M (S.Set Package)
readPackages cabal constraints = do 
  parse_result <- liftIO $ C.parsePackageDescription <$> readFile cabal
  case parse_result of
    (C.ParseFailed fail_msg) ->
      err . show $ fail_msg
    (C.ParseOk warnings desc) ->	do
      unless (L.null warnings) . warning $ 
        preposition 
          "warnings during parse" 
          "of"
          "cabal file"
          "warnings"
          (map show warnings)
      
      toPackages cabal (toDescription desc) constraints 
   where
    -- Produce a simplified description of the cabal file for processing.
    toDescription :: C.GenericPackageDescription -> DependencyDescription 
    toDescription gpd =  
     DependencyDescription
       (S.fromList . map toStrName . C.condTreeConstraints <$> C.condLibrary gpd)
       (toTargets . C.condExecutables $ gpd)
       (toTargets . C.condTestSuites $ gpd)
       (toTargets . C.condBenchmarks $ gpd)
