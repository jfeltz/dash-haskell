module Options.Documentation where
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Indent

data Topic = Topic { title :: String, content :: String } 
instance Eq Topic where
  l == r = title l == title r
instance Ord Topic where
  compare l r = compare (title l) (title r)

type Tag = String 

type Documentation = M.Map Tag Topic 

tags :: Documentation -> [String] 
tags = M.keys 

cabalTopic, providerTopic, outputTopic, packageTopic :: Topic
cabalTopic = 
  Topic "cabal constraints" $
    L.intercalate "\n" [
      "These are constraints on results from a cabal file source",
      "e.g. \"-r executables=foo,excluded=base,ghc\"",
      "possible variables:\n",
      " executables : the cabal executable targets to limit to",
      " suites      : the cabal suite targets to limit to",
      " benchmarks  : the cabal benchmark targets to limit to",
      " library     : the cabal library target to limit to\n",
      " excluded    : packages to avoid using"
   ]
providerTopic = Topic "package database provider" $
  L.intercalate "\n" [
    "the external program to call to produce package databases, e.g:",
    "variables:", 
    " cabal : use cabal sandbox package db's",
    " ghc   : use ghc's package db's",
    " dir   : use package db, this still makes a call to ghc-pkg",
    " e.g. \"-p dir,/home/jpf/.ghc/x86_64-linux-7.8.3/package.conf.d\"",
    " Note, only one provider at once is supported at this time."
    ]
outputTopic= Topic "output" 
 "The directory root of which to write .docset directories to"
packageTopic= Topic "package" $
 L.intercalate "\n" 
  ["a ghc package, e.g. either, or either-4.1.0" ,
  "1. dash-haskell will choose the versioned package if provided", 
  "both unversioned and versioned",
  "2. If the package is unversioned it will choose the first as located by",
  " the package db provider."
  ]

toEntry :: S.Set String -> Topic -> Documentation -> Documentation 
toEntry s t doc = L.foldl (\m' k -> M.insert k t m') doc $ S.toList s 

docs :: Documentation
docs = 
  let fromListing m (t,l) = toEntry (S.fromList l) t m in 
    L.foldl fromListing M.empty [
      (cabalTopic    , ["r", "cabal-constraints"]),
      (providerTopic , ["p", "dbprovider", "provider"]),
      (outputTopic   , ["o", "outputdir", "output", "docset"]),
      (packageTopic  , ["packages", "package"])
    ]

-- | Given an arbitrary stream of options,
-- convert to possible topic indexes for documenation lookup.
toIndexes :: [String] -> [String] 
toIndexes = S.toList . S.fromList . map toTopic  
  where
    toTopic :: String -> String
    toTopic [] = [] 
    toTopic ('-':rest) = toTopic rest 
    toTopic remainder  = remainder 

toHelp :: Documentation -> [String] -> IO () 
toHelp d [] = 
  putStr $ 
    "available help topic tags:\n" ++ L.intercalate "," (tags d) ++ "\n"
toHelp d args = 
  -- fold over each index, producing a list of docs to display 
  let topic_set = L.foldl folded S.empty $ toIndexes args in 
  if S.null topic_set then do 
    putStrLn "sorry, no documentation available for expressions given"
    toHelp docs [] 
  else do
    putStrLn "accessing help on topics: \n"
    putStr . L.intercalate "\n" . L.map headed $ S.toList topic_set
    putStr "\n"
  where 
    headed :: Topic -> String 
    headed topic = ' ':title topic ++ ':':'\n':'\n': 
      fromIndent (content topic) 2

    folded :: S.Set Topic -> String -> S.Set Topic
    folded s index = maybe s (`S.insert` s) $ M.lookup index d
