module Options.CabalConstraints where
import qualified Data.Set as S
import           Text.ParserCombinators.Parsec
import           Control.Monad
import qualified Options.Applicative.Types as O

data CabalConstraints = 
  CabalConstraints {
    lib        :: Bool,
    execs      :: S.Set String, -- The execs to limit to
    suites     :: S.Set String, -- The suites to limit to
    benchmarks :: S.Set String, -- The benchmarks to limit to
    excluded   :: S.Set String -- Packages to exclude
  } deriving (Show, Eq)

instance Monoid CabalConstraints where 
  mempty = CabalConstraints False mempty mempty mempty  mempty
  mappend l r = 
    CabalConstraints 
      (lib r || lib l)  -- This just biases to True 
      (mappend (execs l      ) (execs r)     )
      (mappend (suites l     ) (suites r)    )
      (mappend (benchmarks l ) (benchmarks r))
      (mappend (excluded l   ) (excluded r)  )

none :: CabalConstraints 
none = mempty 

type Constructor = S.Set String -> CabalConstraints

excluded',benchmarks',suites',execs' :: Constructor 
excluded'    s = none { excluded   = s } 
benchmarks'  s = none { benchmarks = s } 
execs'       s = none { execs      = s } 
suites'      s = none { suites     = s } 

-- | Parser for the cabal constraints option.
-- Note: I don't think this can be simplified with sepBy since the 
-- non-sep parser may fail, causing input and positioning to be lost.
-- If someone knows a better way, please advise/patch.
constraints :: Maybe Constructor -> Parser CabalConstraints 
constraints Nothing =
  -- parse a lhs binding or lib 
  choice $ map try [
    -- lib as leader
    do  
      c <- string "library" >> return (none { lib = True })
      choice [
        char ',' >> mappend c <$> constraints Nothing, 
        eof >> return c
        ]
    , do 
        ctor <- binding
        constraints (Just ctor) 
    ]
  where
    binding :: Parser Constructor 
    binding = do 
      ctor <- choice $ map try [ 
        string "executables" >> return execs',
        string "suites"      >> return suites',
        string "benchmarks"  >> return benchmarks',
        string "excluded"    >> return excluded'
        ]
      void . char $ '=' 
      return ctor
constraints (Just ctor) = do
  -- parse the next value
  v <- value  
  -- the next is either buf end, or a deliminated binding/value 
  c <- choice [
    eof >> return none,
    try (char ',' >> constraints Nothing), -- binding pre-empts value
    try (char ',' >> constraints (Just ctor))
    ]
  return $ mappend (ctor . S.singleton $ v) c
  where
    value :: Parser String
    value = do 
      var <- many1 (alphaNum  <|> char '-')
      notFollowedBy (char '=')
      return var

toConstraints :: O.ReadM CabalConstraints
toConstraints = do
    expr <- O.readerAsk
    case parse (constraints Nothing) [] expr of 
      Left err -> O.readerError . show $ err
      Right c -> return c
