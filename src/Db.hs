module Db where
data Db = 
  -- A cabal sandbox db, with optional path to sandbox configuration file  
 Sandbox FilePath
  -- The global ghc package database
 | Global                 
  -- The user package database, as defined by cabal 
 | User                   
  -- A direct database path
 | Path FilePath deriving (Ord, Eq)
 
instance (Show Db) where
  show (Sandbox p) = "cabal sandbox db with config file: " ++ p 
  show (Global   ) = "ghc distribution (global) db" 
  show (User     ) = "system user db" 
  show (Path    p) = "db path: " ++ p
