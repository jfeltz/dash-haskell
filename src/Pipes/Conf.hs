module Pipes.Conf where
import           Control.Monad
import           Control.Monad.M
import           Control.Monad.Trans.Either
import qualified Data.List as L
import           Data.Maybe
import           Data.String.Util
import           Distribution.InstalledPackageInfo as DIP
import qualified Filesystem.Path.CurrentOS as P
import           Package.Conf
import           Pipes
import qualified System.Directory as D
       
confError :: FilePath -> String -> M r
confError path fatal = 
  err $ preposition "parse errors" "in" "conf file" path [fatal] 
    
fromParseResults :: FilePath -> ParseResult InstalledPackageInfo -> M Conf
fromParseResults conf (ParseFailed _) = 
  confError conf "package conf file parse failed"
fromParseResults conf (ParseOk cabalWarnings fields)
  | null (haddockHTMLs fields)      =
    confError conf "failed to extract html field from conf file"
  | null (haddockInterfaces fields) = 
    confError conf "failed to extract haddock interface field from conf file"
  | otherwise = do
    unless (L.null cabalWarnings) $
      liftIO . putStr $
        "parse of:\n " ++ conf ++ "\n completed with warnings: " 
        ++ L.intercalate "\n" (map show cabalWarnings) 
    lift . right $
      Conf 
        -- We're not using Cabal's type information beyond just
        -- extracting package data. Ghc types are used for the rest.
        (sourcePackageId fields)
        
        -- TODO Respect multiple interfaces, however this is not the common
        -- consensus for use of haddock interfaces. 
        (P.decodeString (head $ haddockInterfaces fields))
        (P.decodeString $ head (haddockHTMLs fields) ++ "/")
        (DIP.exposed fields)

checkPath :: Bool -> P.FilePath -> String -> IO (Maybe String) 
checkPath dir path name = do 
  exists <- predicate . P.encodeString $ path 
  return $ if exists then Nothing else Just $ "missing: " ++ name ++ ' ':msg'
  where 
    (predicate, msg') =
      if dir then (D.doesDirectoryExist, "dir") else (D.doesFileExist, "file")

diagnosePaths :: Conf -> M [String]
diagnosePaths conf = do
  (html_doc_warnings, interface_warnings) <- liftIO $  
    liftM2 (,)
      (checkPath True (htmlDir conf) "html doc")
      (checkPath False (interfaceFile conf) "haddock interface")
  return $ catMaybes [interface_warnings, html_doc_warnings] 

pipe_Conf :: PipeM FilePath Conf ()
pipe_Conf = forever $ do
  pkg_db_conf <- await
  -- Retrieve the package conf file from the package db
  parse_results <- liftIO $
    DIP.parseInstalledPackageInfo <$> Prelude.readFile pkg_db_conf
  c <- lift $ fromParseResults pkg_db_conf parse_results

  errors <- lift $ diagnosePaths c
  if L.null errors then
    yield c
  else 
    lift $
      do msg "\n"
         warning $ "failed to process: " ++ show (pkg c)
         warning $ preposition "path errors" "in" "pkg conf file" pkg_db_conf errors
         msg "\n" 
