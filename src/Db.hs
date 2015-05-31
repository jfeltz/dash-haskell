module Db where

data Db = 
  -- A cabal sandbox db, with optional path to sandbox configuration file  
 Sandbox
  -- The global ghc package database
 | Global                 
  -- The user package database, as defined by cabal 
 | User                   
  -- A direct database path
 | Dir FilePath
 
instance (Show Db) where
  show (Sandbox) = "cabal sandbox" 
  show (Global ) = "ghc distribution (global db)" 
  show (User   ) = "system user" 
  show (Dir  p)  = "db directory: " ++ p

instance (Ord Db) where
  compare Sandbox Sandbox   = EQ 
  compare Global  Global    = EQ 
  compare User    User      = EQ 
  compare (Dir _) (Dir _) = EQ

  compare (Dir _) Global   = LT 
  compare (Dir _) User     = LT 
  compare (Dir _) Sandbox  = LT 
  compare User Global       = LT 
  compare User Sandbox      = LT 
  compare Global Sandbox    = LT 

  -- Just reversing the above for l != r
  compare Global  (Dir _)  = GT
  compare User    (Dir _)  = GT
  compare Sandbox (Dir _)  = GT
  compare Global  User      = GT
  compare Sandbox Global    = GT
  compare Sandbox User      = GT

instance (Eq Db) where
  l == r = compare l r == EQ
