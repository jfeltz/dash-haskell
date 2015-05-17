module Data.String.Util where
import qualified Data.List as L

preposition :: String -> String -> String -> String -> [String] -> String 
preposition problem prep subjectLabel subject problems = 
  L.intercalate "\n" $
    (problem ++ ' ':prep ++ ' ':subjectLabel ++ ":") 
    : (' ':subject)
    : "with problem(s):" : L.lines (fromIndent (L.unlines problems) 1)

fromIndent ::  String -> Int -> String
fromIndent orig margin = 
  L.unlines . L.map (L.replicate margin ' ' ++)  . L.lines $ orig  
