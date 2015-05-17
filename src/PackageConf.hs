module PackageConf where
import qualified Module as Ghc

data Conf = Conf
  { pkg           :: Ghc.PackageKey
  , interfaceFile :: FilePath -- interface, i.e. .haddock file
  , htmlDir       :: FilePath -- root html source directory
  , exposed       :: Bool     -- module exposure flag
  }
