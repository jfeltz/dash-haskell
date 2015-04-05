module Haddock.Artifact where
import           Control.Monad.IO.Class
import           Control.Monad.M
import           Data.String.Util
import           Documentation.Haddock
import           Distribution.ModuleName (ModuleName)
import           Distribution.Package (PackageIdentifier)
import qualified Filesystem.Path.CurrentOS as P
import qualified Name as Ghc
import qualified Module as Ghc

-- This ADT approach is heavily influenced by philopen's haddocset:
-- https://github.com/philopon/haddocset

data Artifact
  = Haddock P.FilePath -- Note, this is not yet honored. TODO
  | Package  
  | Module   ModuleName 
  | Function ModuleName Ghc.Name

parseError :: String -> P.FilePath -> M r
parseError e p = 
  err $ preposition "parser error" "in" "haddock interface" (P.encodeString p) [e]

fromInterfaces :: PackageIdentifier -> [InstalledInterface] -> [Artifact] 
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
   
toArtifacts :: PackageIdentifier -> P.FilePath -> M [Artifact]
toArtifacts pkg haddock' = do 
  interface_file <- liftIO $ readInterfaceFile freshNameCache (P.encodeString haddock')
  case interface_file of
    Left e -> parseError e haddock'
    Right (InterfaceFile _ installed_ifaces) ->
      return $ Haddock haddock' : Package : fromInterfaces pkg installed_ifaces
