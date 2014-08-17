module Package where
import Data.Char

unversioned :: String -> String
unversioned [] = 
  [] 
unversioned ('-':c:rest) =
  if isDigit c then [] else '-':c:unversioned rest 
unversioned (c:rest) = 
  c:unversioned rest
