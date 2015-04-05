module Package.Conf where
import qualified Filesystem.Path.CurrentOS as P
import Distribution.Package (PackageIdentifier)

data Conf = Conf
  { pkg           :: PackageIdentifier
  , interfaceFile :: P.FilePath -- interface, i.e. .haddock file
  , htmlDir       :: P.FilePath -- root html source directory
  , exposed       :: Bool -- module exposure flag
  }
