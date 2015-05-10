module FilePath where

import qualified Filesystem.Path.CurrentOS as P
import qualified System.Directory as D

checkPath :: Bool -> P.FilePath -> String -> IO (Maybe String) 
checkPath dir path name = do 
  exists <- predicate . P.encodeString $ path 
  return $ if exists then Nothing else Just $ "missing: " ++ name ++ ' ':msg'
  where 
    (predicate, msg') =
      if dir then (D.doesDirectoryExist, "dir") else (D.doesFileExist, "file")

