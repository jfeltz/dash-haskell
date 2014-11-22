module Options.DbProvider where
import qualified Data.List as L
import Options.Applicative.Types
import Control.Applicative
import Control.Monad
import Data.Maybe

data DbProvider = 
  CabalSandbox (Maybe String)
  | Ghc (Maybe String) 
  | Db FilePath  

instance (Show DbProvider) where
  show dbp = 
    let (cmd, args) = toExec dbp in 
      L.intercalate "\n"
       ["lookup strategy: " ++  desc, "cmd: " ++ cmd ++ "\nargs: " ++ unwords args] 
    where
      desc = 
        case dbp of 
          CabalSandbox _ -> "cabal sandbox db index" 
          Ghc _          -> "ghc db index" 
          Db _           -> "ghc db index with directory narrowing" 

fromSplit :: Char -> String -> ReadM (String, Maybe String)
fromSplit c opt = 
  case opt of 
    []         -> return ([], Nothing) 
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

toExec :: DbProvider -> (String, [String])
toExec (CabalSandbox args) = 
  (,) "cabal" $ ["sandbox", "hc-pkg", "list"] ++ maybeToList args
toExec (Ghc args)          = 
  (,) "ghc-pkg" $ "list" : maybeToList args
toExec (Db fp)             = 
  (,) "ghc-pkg" ("list":["--package-db=" ++ fp]) 

toProvider :: ReadM DbProvider
toProvider = do
  expr <- readerAsk
  (prov, arg) <- fromSplit ',' expr
  join $ constructor prov <*> pure arg
  where 
    constructor :: String -> ReadM (Maybe String -> ReadM DbProvider) 
    constructor prov =
     maybe (readerError "invalid db provider") return f
     where 
      f :: Maybe (Maybe String -> ReadM DbProvider)
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
