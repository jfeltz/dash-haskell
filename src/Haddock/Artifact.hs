module Haddock.Artifact where
import           Control.Monad.IO.Class
import           Control.Monad.M
import           Data.String.Util
import           Documentation.Haddock
import           Distribution.ModuleName (ModuleName)
-- import           Distribution.Package (PackageIdentifier)
-- import           System.FilePath
import qualified Module as Ghc
import qualified Name as Ghc

-- This ADT approach is heavily influenced by philopen's haddocset:
-- https://github.com/philopon/haddocset

data Artifact
  = Haddock FilePath -- This is not yet honored. (TODO)
  | Package  
  | Module   ModuleName 
  | Function ModuleName Ghc.Name

parseError :: String -> FilePath -> M r
parseError e p = 
  err $ preposition "parser error" "in" "haddock interface" p [e]

fromInterfaces :: Ghc.PackageKey -> [InstalledInterface] -> [Artifact] 
fromInterfaces _   []       = []  
fromInterfaces pkg (i:rest) =
   let module_str = Ghc.moduleNameString . Ghc.moduleName $ instMod i in
     if OptHide `notElem` instOptions i then 
      (Module . read $ module_str ) :
       foldl 
         (\a e -> Function (read module_str) e : a)
         -- append to artifacts from rest of installed interfaces
         (fromInterfaces pkg rest) 
         (instVisibleExports i)
     else
       fromInterfaces pkg rest 
   
toArtifacts :: Ghc.PackageKey -> FilePath -> M [Artifact]
toArtifacts pkg haddock' = do 
  interface_file <- liftIO $ readInterfaceFile freshNameCache haddock'
  case interface_file of
    Left e                                   ->
      parseError e haddock'
    Right (InterfaceFile _ installed_ifaces) ->
      return $ Haddock haddock' : Package : fromInterfaces pkg installed_ifaces
