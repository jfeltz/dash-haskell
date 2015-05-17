module Options.Db where
import qualified Data.List as L
import           Data.Maybe
import           Db
import qualified Options.Applicative.Types as O
import           Text.ParserCombinators.Parsec

-- | TODO global is disallowed, and that
-- needs to be documented.
  
dbStrings :: [String] 
dbStrings = ["sandbox", "user", "path"]

dbPaths :: [Db] 
dbPaths = [Sandbox [], User, Path []]
        
fromString :: String -> Db
fromString str = fromJust . L.lookup str $ zip dbStrings dbPaths 

valid :: [Db] -> Bool 
valid []  = False 
valid dbs = L.length (L.nub dbs) == L.length dbs 
  
quasiOrdering :: Parser [Db] 
quasiOrdering =
  map fromString <$> sepBy (choice $ map string dbStrings) (char ',') 

defaultOrdering :: [Db]
defaultOrdering = dbPaths 

toOrdering :: O.ReadM [Db]
toOrdering = do 
  expr <- O.readerAsk
  case parse quasiOrdering [] expr of 
    Left err -> O.readerError . show $ err
    Right qo ->
      if valid qo then
        return qo
      else 
        O.readerError "invalid (possibly containing duplicates)"

