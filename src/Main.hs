-- import           Control.Monad.M

-- import           Pipes
-- import           Pipe.FileSystem
-- import           Pipe.Conf
-- import           Pipe.Db
import           Options.Applicative
import           Options.Documentation
import qualified Options as O

import qualified Data.List as L
import           System.Environment
import           Db
import           Data.Maybe
                 
cabalSandboxConfig :: FilePath  
cabalSandboxConfig = "./cabal.sandbox.config" 

defaultStack :: [Db]
defaultStack = [Global, User]
             
-- | The default stack defined when cabal is sourced for packages
cabalStack :: (Db, [Db])
cabalStack = (Sandbox cabalSandboxConfig, defaultStack)
           
toMaybe :: Bool -> a -> Maybe a
toMaybe True a  = Just a 
toMaybe False _ = Nothing 

matched :: [Db] -> [Db] -> Bool 
matched []      []           = True 
matched (d:dbs) (o:ordering) = predicate d o && matched dbs ordering where
  predicate (Sandbox _) (Sandbox _) = True
  predicate (Global   ) (Global   ) = True
  predicate (User     ) (User     ) = True
  predicate (Path    _) (Path    _) = True
  predicate _           _           = False 
   
-- | produce an ordering of db's from the options, or error
dbs :: O.Options -> Either String [Db]
dbs opts = 
  if matched actual (O.dbOrdering opts) then 
    Right actual
  else 
    Left "couldn't match provided dbs with expected from ordering" 
  where 
    actual :: [Db]
    actual = 
      catMaybes $ 
        (Sandbox . fromMaybe cabalSandboxConfig <$> O.sandbox opts) : 
          L.map (uncurry toMaybe)
            [(O.global opts, Global), (O.user opts, User)]

main :: IO ()
main = do 
  -- Check for help mode arg first. There doesn't seem to be a good way to do 
  -- this otherwise with opt-parse applicative.
  args <- getArgs
  case L.partition (== "help") args of 
    ([], args') -> do
      options <- handleParseResult $ execParserPure (prefs idm) parserInfo args'
      return ()
      
      -- Run the package processing pipeline. Packages that can't be
      -- completed due -- to either conversion error or user error, should, if
      -- necessary, leave a safe partially -- completed state on the FS that
      -- can be handled by dependant tools, e.g. Emacs helm-dash.

      -- runM (newEnv (not . quiet $ options)) . runEffect $
      --   -- writes converted html, haddock, and sql db
      --   cons_writeFiles (P.decodeString $ outputDir options) 
      --   <-< pipe_Conf                         -- yields vetted package configs
      --   <-< pipe_ConfFp (dbprovider options)  -- yields GHC package config files
      --   <-< prod_Dependencies options         -- yields packages from options
    (_, rest) -> toHelp docs rest
  
  where 
   parserInfo :: ParserInfo O.Options
   parserInfo = info (helper <*> O.parser)  $
     header "dash-haskell v1.1.0.0, a dash docset construction tool for Haskell packages"
     <> progDesc "additional help is available with \"dash-haskell help <topic|option>\""
     <> footer "http://www.github.com/jfeltz/dash-haskell (C) John P. Feltz 2014, 2015"
