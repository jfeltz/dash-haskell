module PackageConf where
import qualified Module as Ghc
import FilePath
import Data.Maybe (catMaybes)

data PackageConf = PackageConf
  { pkg           :: Ghc.UnitId
  , interfaceFile :: FilePath -- interface, i.e. .haddock file
  , htmlDir       :: FilePath -- root html source directory
  , exposed       :: Bool     -- module exposure flag
  }

problems :: PackageConf -> IO [String]
problems conf =
  catMaybes <$> sequence [
    checkDir (htmlDir conf) "haddock html"
    , checkFile (interfaceFile conf) "haddock interface"
    ]
