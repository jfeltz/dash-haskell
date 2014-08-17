module Data.String.Util (preposition) where
import Data.String.Indent
import qualified Data.List as L

preposition :: String -> String -> String -> String -> [String] -> String 
preposition problem prep subjectLabel subject problems = 
  L.intercalate "\n" $
    (problem ++ ' ':prep ++ ' ':subjectLabel ++ ":") 
    : (' ':subject)
    : "with problem(s):" : L.lines (fromIndent (L.unlines problems) 1)
