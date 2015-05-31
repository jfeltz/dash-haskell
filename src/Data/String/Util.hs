{-# LANGUAGE 
  UndecidableInstances, FlexibleInstances, TypeSynonymInstances
  #-}
module Data.String.Util where
import qualified Data.List as L

-- Nasty workaround for handling (Show a) uniformly with strings
class Unquoted a where
  toString :: a -> String
instance {-# OVERLAPPING #-} Unquoted String where
  toString = id
instance {-# OVERLAPPING #-} Unquoted Char where
  toString x = [x]
instance Show a => Unquoted a where
  toString = show   

listing :: (Unquoted u) => [u] -> String 
listing = L.intercalate "\n" . map toString

preposition :: String -> String -> String -> String -> [String] -> String 
preposition problem prep subjectLabel subject problems = 
  listing $
    (problem ++ ' ':prep ++ ' ':subjectLabel ++ ":") 
    : (' ':subject)
    : "with problem(s):" : L.lines (indenting 1 $ L.unlines problems)

indenting ::  Int -> String -> String
indenting margin orig = 
  L.unlines . L.map (L.replicate margin ' ' ++)  . L.lines $ orig  
