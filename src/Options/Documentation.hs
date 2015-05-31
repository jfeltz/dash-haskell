{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Options.Documentation where
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Util

data Topic = Topic { title :: String, content :: String } 
instance Eq Topic where
  l == r = title l == title r
instance Ord Topic where
  compare l r = compare (title l) (title r)

type Tag = String 

type Documentation = M.Map Tag Topic 

tags :: Documentation -> [String] 
tags = M.keys 

cabalSourcingTopic = 
  Topic "cabal sourcing" $
    listing [
      "This command pulls package selections from a cabal file",
      "  e.g. -c <project.cabal>\n",
      "Note:",
      "This command is not the same as a db provider.", 
      "Note:",
      "If the package occurs more than once in the cabal file", 
      "with different range, neither package is taken.\n", 
      "Pulled packages can be constrained, see: cabal-constraints." 
      ]

outputTopic =
  Topic "output" $ listing [
     "For each package sourced by dash-haskell, a matching docset"
   , "will be written under this directory in its version, e.g:"
   , " \"output/package-1.2.3.docset\""
  ]

nouserTopic =
  Topic "no-user" $ listing [
    "the no-user flag removes the user package db, e.g. :"
    , "~/.ghc/x86_64-linux-7.10.1/package.conf.d/"
    , "from db consideration"
    ]

dbTopic   =
  Topic "package database directory" $ listing [
    "a db directory to directly use as the package database,",
    "this is ordered as a \'dir\' in the db stack"
    ]

orderingTopic   =
  Topic "ordering" $ listing [
    "an ordering of package db's, in that this is the order in which",
    "dbs are examined for packages to use for documentation conversion",
    "\n",
    "Note: The global db must always appear, and it always first. This",
    "is due to upstream constraints in ghc right now. (05/30/205)."
    ]

packageTopic =
  Topic "package" $ "a ghc package, e.g. either, or either-4.1.0"

toEntry :: S.Set String -> Topic -> Documentation -> Documentation 
toEntry s t doc = L.foldl (\m' k -> M.insert k t m') doc $ S.toList s 

docs :: Documentation
docs = 
  let fromListing m (t,l) = toEntry (S.fromList l) t m in 
    L.foldl fromListing M.empty [
      (cabalSourcingTopic    , ["c", "cabal"]),
      (outputTopic           , ["o", "outputdir", "output", "docset"]),
      (packageTopic          , ["packages", "package"]),
      (nouserTopic           , ["n", "no-user", "user"]),
      (dbTopic               , ["db", "dir"]),
      (orderingTopic         , ["d", "ordering"])
      ]

-- | Given an arbitrary stream of options,
-- convert to possible topic indexes for documentation look-up.
toIndexes :: [String] -> [String] 
toIndexes = S.toList . S.fromList . map toTopic  
  where
    toTopic :: String -> String
    toTopic [] = [] 
    toTopic ('-':rest) = toTopic rest 
    toTopic remainder  = remainder 

toHelp :: Documentation -> [String] -> IO () 
toHelp d [] = 
  putStrLn $ "available help topics:\n" ++ L.intercalate ", " (tags d)
toHelp d args = 
  -- fold over each index, producing a list of docs to display 
  let topic_set = L.foldl folded S.empty $ toIndexes args in 
  if S.null topic_set then do 
    putStrLn "Sorry, no documentation available for expressions given.\n"
    toHelp docs [] 
  else do
    putStrLn "accessing help on topics,"
    putStrLn $ indenting 2 (listing . L.map headed $ S.toList topic_set)
  where 
    headed :: Topic -> String 
    headed topic =
      ' ':title topic ++ ':':'\n':'\n': indenting 2 (content topic)

    folded :: S.Set Topic -> String -> S.Set Topic
    folded s index = maybe s (`S.insert` s) $ M.lookup index d
