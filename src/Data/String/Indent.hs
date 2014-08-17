module Data.String.Indent (fromIndent) where
import qualified Data.List as L

fromIndent ::  String -> Int -> String
fromIndent orig margin = 
  L.unlines . L.map (L.replicate margin ' ' ++)  . L.lines $ orig  
