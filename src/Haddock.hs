{-# LANGUAGE OverloadedStrings #-}
module Haddock where
import qualified Filesystem.Path.CurrentOS as FPC
import Documentation.Haddock

data HaddockArg = HaddockArg { name :: String, value :: Maybe String } 

instance Show HaddockArg where
  show arg = "--" ++ name arg ++ maybe [] (\v -> '=':v) (value arg) 

runHaddock :: [HaddockArg] -> IO () 
runHaddock = haddock . map show 

flag :: String -> HaddockArg
flag = flip HaddockArg Nothing

readInterface :: FPC.FilePath -> HaddockArg 
readInterface ipath = 
  HaddockArg "read-interface" . Just $ 
    (FPC.encodeString . FPC.filename . FPC.dropExtension $ ipath) ++ ',': FPC.encodeString ipath

-- | Given a directory with .haddock interface specs, 
-- write index files and TOC for those to the document directory. 
runHaddockIndex :: FPC.FilePath -> FPC.FilePath -> IO ()
runHaddockIndex interface document_dir =
  runHaddock $ [
    -- generate html index file(s)
    flag "gen-index", 
    -- generate the html contents file
    flag "gen-contents",
    -- output directory for index file(s) is same as document dir 
    HaddockArg "odir" . Just . FPC.encodeString $ document_dir
   ] ++ [readInterface interface]
