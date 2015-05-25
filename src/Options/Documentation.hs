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

cabalConstraintsTopic, cabalSourcingTopic, outputTopic, packageTopic :: Topic
cabalConstraintsTopic = 
  Topic "cabal constraints" $
    listing [
      "These are constraints on packages pulled from a cabal file source.",
      "These constraints allow one to simply select subsets of cabal targets to use",
      "and subsets of packages to avoid sourcing.\n",
      "Some options support multivalue bindings, and are of the form:",
      " -r <var=value>,<var2=value1,value2>",
      "e.g. \"-r executables=foo,excluded=base,ghc\"",
      "\npossible variables:\n",
      " executables : the executable targets to limit to",
      " suites      : the suite targets to limit to",
      " benchmarks  : the benchmark targets to limit to",
      " library     : limit to library, this takes no arguments\n",
      " excluded    : unversioned packages to avoid using"
      ]

cabalSourcingTopic = 
  Topic "cabal sourcing" $
    listing [
      "This command pulls package selections from a cabal file",
      "  e.g. -c <project.cabal>\n",
      "Note:",
      "This command is not the same as a db provider.", 
      "This only uses package names and versions from the cabal file,", 
      "and doesn't process any information from Package Db's, other than what",
      "cabal uses internally to determine available versions.\n",
      "Note:",
      "If the package occurs more than once in the cabal file", 
      "with differint range, neither package is taken", 
      "Pulled packages can be constrained, see: cabal-constraints." 
      ]

-- providerTopic = Topic "package database provider" $
--   listing [
--     "The external program to call to produce package databases\n"
--     , "options are the form <var,args>" 
--     , "e.g:" 
--     , "    -p ghc,'--user'"
--     , " or -p dir,/home/jpf/.ghc/x86_64-linux-7.8.3/package.conf.d\n"
--     , "Note, only one provider at once is supported at this time.\n"
--     , "pairings for <var,args>:"
--     , " var                                        args "
--     , "-----------------------------------------   ----------------------------"
--     , " cabal : use sandbox topped db stack         non-default cabal sandbox"
--     , "                                             config path"
--     , " ghc   : use ghc's package db stack          additional flag string to"
--     , "                                             pass to ghc-pkg"
--     , " dir   : use package db dir directory        the package db directory"
--     ]

outputTopic= Topic "output" $ 
 listing [
   "The directory root of which to write .docset directories to\n"
   , "For each package sourced by dash-haskell, a matching docset"
   , "will be written to that directory in its full version form, e.g:\n"
   , "  \"output/package-1.2.3.docset\""
 ]

packageTopic= Topic "package" $
  "a ghc package, e.g. either, or either-4.1.0"

toEntry :: S.Set String -> Topic -> Documentation -> Documentation 
toEntry s t doc = L.foldl (\m' k -> M.insert k t m') doc $ S.toList s 

docs :: Documentation
docs = 
  let fromListing m (t,l) = toEntry (S.fromList l) t m in 
    L.foldl fromListing M.empty [
      (cabalSourcingTopic    , ["c", "cabal"]),
      (cabalConstraintsTopic , ["r", "cabal-constraints"]),
      -- (providerTopic         , ["p", "dbprovider", "provider"]),
      (outputTopic           , ["o", "outputdir", "output", "docset"]),
      (packageTopic          , ["packages", "package"])
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
