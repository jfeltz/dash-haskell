module Options.DbStack where
import           Control.Monad
import qualified Data.List as L

import           Options.Applicative.Types

data DbStack = Sandbox (Maybe String) | Ghc (Maybe String) | Single FilePath

-- instance (Show DbStack) where
--   show dbp = 
--     let (cmd, args) = toExec dbp in 
--       L.intercalate "\n"
--        ["lookup strategy: " ++  desc, "cmd: " ++ cmd, "args: " ++ unwords args] 
--     where
--       desc = 
--         case dbp of 
--           CabalSandbox _ -> "cabal sandbox db index" 
--           Ghc          _ -> "ghc db index" 
--           Db           _ -> "ghc db index with directory narrowing" 

fromSplit :: Char -> String -> ReadM (String, Maybe String)
fromSplit c opt = 
  case opt of 
    []      -> return ([], Nothing) 
    (s:str) ->
     if s == c then do
       param <- fromParam str 
       return ([], param)
     else do 
       (l, r) <- fromSplit c str
       return (s:l, r)
  where 
    fromParam []      =  return Nothing
    fromParam (c':str)=
      if c' == c then
        readerError $ "encountered delimeter(" ++ c:") twice"
      else
        Just . maybe [c'] (c':) <$> fromParam str

-- toExec :: DbStack -> (String, [String])
-- toExec (CabalSandbox args) =
--   (,) "cabal" $ ["sandbox", "hc-pkg", "list"] ++ maybeToList args
-- toExec (Ghc args)          = 
--   (,) "ghc-pkg" $ "list" : maybeToList args
-- toExec (Db fp)             = 
--   (,) "ghc-pkg" ("list":["--package-db=" ++ fp]) 

toProvider :: ReadM DbStack
toProvider = do
  expr <- readerAsk
  (prov, arg) <- fromSplit ',' expr
  join $ constructor prov <*> pure arg
  where 
    constructor :: String -> ReadM (Maybe String -> ReadM DbStack) 
    constructor prov =
     maybe (readerError "invalid db provider") return f
     where 
      f :: Maybe (Maybe String -> ReadM DbStack)
      f = 
        -- return a constructor given an arg
        L.lookup prov
         [("ghc"   , return . Ghc),
          ("cabal" , return . CabalSandbox),
          ("dir"   , 
            maybe 
              (readerError "requires directory path")
              (return . Db))
          ]
