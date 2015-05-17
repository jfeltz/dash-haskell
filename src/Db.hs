module Db where

data Db = 
  -- A cabal sandbox db, with optional path to sandbox configuration file  
 Sandbox FilePath
  -- The global ghc package database
 | Global                 
  -- The user package database, as defined by cabal 
 | User                   
  -- A direct database path
 | Path FilePath deriving (Ord)
 
instance (Show Db) where
  show (Sandbox p) = "cabal sandbox db with config file: " ++ p 
  show (Global   ) = "ghc distribution (global) db" 
  show (User     ) = "system user db" 
  show (Path    p) = "db path: " ++ p

instance (Eq Db) where
  (Sandbox _) == (Sandbox _) = True 
  (Global   ) == (Global   ) = True 
  (User     ) == (User     ) = True 
  (Path    _) == (Path    _) = True
  _           == _           = False 

-- instance (Ord Db) where
--   compare (Sandbox _)  (Sandbox _) = EQ 
--   compare (Global  _)  (Global  _) = EQ 
--   compare (User    _)  (User    _) = EQ 
--   compare (Path    _)  (Path    _) = EQ
