module Package.Conf where
import Distribution.Package (PackageIdentifier)

data Conf = Conf
  { pkg           :: PackageIdentifier
  , interfaceFile :: FilePath -- interface, i.e. .haddock file
  , htmlDir       :: FilePath -- root html source directory
  , exposed       :: Bool -- module exposure flag
  }
