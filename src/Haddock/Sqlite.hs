{-# LANGUAGE OverloadedStrings #-}
module Haddock.Sqlite where
import           Control.Monad.IO.Class
import           Control.Monad.M

import qualified Data.Text as T
import           Database.SQLite.Simple
import           Haddock.Artifact
import qualified Name as Ghc
import qualified Module as Ghc
import Distribution.ModuleName
import Distribution.Text

data IndexRow = IndexRow {
  nameAttr :: T.Text 
  , typeAttr :: T.Text 
  , pathAttr :: T.Text 
  ,  modAttr :: T.Text 
} deriving (Show)

instance Monoid IndexRow where
  mempty = IndexRow mempty mempty mempty mempty
  mappend l r =  
    IndexRow 
      (mappend (nameAttr l) (nameAttr r))
      (mappend (typeAttr l) (typeAttr r))
      (mappend (pathAttr l) (pathAttr r))
      (mappend (modAttr l)  (modAttr r))

instance ToRow IndexRow where
  toRow index = 
    [  SQLText . nameAttr $ index
     , SQLText . typeAttr $ index
     , SQLText . pathAttr $ index
     , SQLText . modAttr  $ index
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

modStr :: ModuleName -> String 
modStr = display 

modUrl :: ModuleName -> String
modUrl = map (\c-> if c == '.' then '-' else c) . modStr 

escapeSpecial :: String -> String
escapeSpecial = 
  concatMap 
    (\c -> if c `elem` specialChars then '-': show (fromEnum c) ++ "-" else [c])
  where
    specialChars :: String
    specialChars = "!&|+$%(,)*<>-/=#^\\?"
 
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
    -- | Convert haddock artifacts to attributes for table update.
    toAttributes = 
      case art of 
       Haddock _              ->
         -- TODO unsupported right now 
         return Nothing 
       Package                ->  
         return . Just $ 
           (Ghc.packageKeyString p, "Package", "index.html", [])
       Module mod_name        -> 
         return . Just $
            (modStr mod_name, 
             "Module" , 
             modUrl mod_name ++ ".html" ,
             show mod_name)
       Function mod' ghc_name -> 
         let (declType, pfx) = toPair ghc_name in
           return . Just $ 
             (modStr mod' ++ '.': Ghc.getOccString ghc_name,
              declType, 
              url pfx,
              modStr mod')
          where
            url pfx =
              modUrl mod' ++ ".html#" ++ pfx : ':' : 
                escapeSpecial (Ghc.getOccString ghc_name)
            toPair n
              | Ghc.isTyConName   n = ("Type"        , 't')
              | Ghc.isDataConName n = ("Constructor" , 'v')
              | otherwise           = ("Function"    , 'v')
