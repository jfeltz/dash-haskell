module Haddock.Artifact where
import           Control.Monad.IO.Class
import           Control.Monad.M
import           Data.String.Util
import           Documentation.Haddock
import qualified Module as Ghc
import qualified Name as Ghc

-- This ADT approach is heavily influenced by philopen's haddocset package:
-- https://github.com/philopon/haddocset

data Artifact
  = Haddock  FilePath -- Note, this is not yet honored. TODO
  | Package  
  | Module   Ghc.Module
  | Function Ghc.Module Ghc.Name

parseError :: String -> FilePath -> M r
parseError e p = 
  err $ preposition "parser error" "in" "haddock interface" p [e]

fromInterfaces :: Ghc.PackageKey -> [InstalledInterface] -> [Artifact]
fromInterfaces _   []       = []  
fromInterfaces pkg (i:rest) =
   let moduleName = instMod i in
     if OptHide `notElem` instOptions i then 
      Module moduleName : 
       foldl 
         (\a e -> Function moduleName e : a)
         -- append to artifacts from rest of installed interfaces
         (fromInterfaces pkg rest) 
         (instVisibleExports i)
     else
       fromInterfaces pkg rest 
   
toArtifacts :: Ghc.PackageKey -> FilePath -> M [Artifact]
toArtifacts pkg haddock' = do 
  interface_file <- liftIO $ readInterfaceFile freshNameCache haddock'
  case interface_file of
    Left e -> parseError e haddock'
    Right (InterfaceFile _ installed_ifaces) ->
      return $ Haddock haddock' : Package : fromInterfaces pkg installed_ifaces
