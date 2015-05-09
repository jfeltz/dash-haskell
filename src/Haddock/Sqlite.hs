{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Haddock.Sqlite where
import           Control.Monad.IO.Class
import           Control.Monad.M
import           Data.Monoid
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Haddock.Artifact
import qualified Module as Ghc
import qualified Name as Ghc

data IndexRow = IndexRow {
  nameAttr :: T.Text 
  , typeAttr :: T.Text 
  , pathAttr :: T.Text 
  , modAttr :: T.Text 
} deriving (Show)

instance Monoid IndexRow where
  mempty = IndexRow mempty mempty mempty mempty
  mappend l r =  
    IndexRow 
      (mappend (nameAttr l) (nameAttr r))
      (mappend (typeAttr l) (typeAttr r))
      (mappend (pathAttr l) (pathAttr r))
      (mappend (modAttr l) (modAttr r))

-- TODO lensify
instance ToRow IndexRow where
  toRow index = 
    [SQLText . nameAttr $ index
     , SQLText . typeAttr $ index
     , SQLText . pathAttr $ index
     , SQLText . modAttr $ index
    ]
  
-- I probably chould derive this from a type, but that's overkill right now.
table :: String
table = "searchIndex(name, type, path, module)"

createTable :: Connection -> IO ()
createTable conn =
  mapM_ (execute_ conn) 
    ["CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT, module TEXT);",
      Query . T.pack $ "CREATE UNIQUE INDEX anchor ON " ++ table ++ ";"]

insertRow :: Connection -> IndexRow -> IO ()
insertRow conn =
  execute conn 
    (Query . T.pack $ "INSERT OR IGNORE INTO " ++ table ++ " VALUES (?,?,?,?);")

modUrl :: Ghc.Module -> String
modUrl = 
  map (\c-> if c == '.' then '-' else c)
    . Ghc.moduleNameString 
    . Ghc.moduleName 

escapeSpecial :: String -> String 
escapeSpecial = 
  concatMap (\c -> if c `elem` specialChars then '-': show (fromEnum c) ++ "-" else [c])
  where
    specialChars :: String = "!&|+$%(,)*<>-/=#^\\?"
 
-- | Update the sqlite database with the given haddock artifact.
fromArtifact :: Ghc.PackageKey -> Connection -> Artifact -> M ()
fromArtifact p conn art = do
  attributes <- toAttributes
  case attributes of 
    Just (name, type', path, m) -> 
      liftIO . insertRow conn $ 
        mempty { 
          nameAttr = T.pack name
          , typeAttr = type'
          , pathAttr = T.pack path
          , modAttr = T.pack m
          }
    Nothing -> 
      return ()
  where
    modStr m = Ghc.moduleNameString $ Ghc.moduleName m
    -- | Convert haddock artifacts to attributes for table update.
    toAttributes = 
      case art of 
       Haddock _               ->
         -- TODO unsupported right now 
         return Nothing 
       Package                 ->  
         return . Just $ 
           (Ghc.packageKeyString p, "Package", "index.html", [])
       Module ghcmod           -> 
         return . Just $
            (modStr ghcmod, "Module" , modUrl ghcmod ++ ".html" , modStr ghcmod)
       Function ghcmod ghcname -> 
         let (declType, pfx) = toPair ghcname in
           return . Just $ 
             ( modStr ghcmod ++ '.':Ghc.getOccString ghcname,
              declType, 
              url pfx,
              modStr ghcmod)
          where
            url pfx =
              modUrl ghcmod ++ ".html#" ++ pfx : ':' : 
                escapeSpecial (Ghc.getOccString ghcname)
            toPair n
              | Ghc.isTyConName   n = ("Type"        , 't')
              | Ghc.isDataConName n = ("Constructor" , 'v')
              | otherwise           = ("Function"    , 'v')
