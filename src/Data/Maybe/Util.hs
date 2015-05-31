module Data.Maybe.Util where

toMaybe :: Bool -> a -> Maybe a
toMaybe True a  = Just a
toMaybe False _ = Nothing 
