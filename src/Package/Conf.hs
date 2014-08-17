module Package.Conf where
import qualified Module as Ghc
import qualified Filesystem.Path.CurrentOS as P

data Conf = Conf
  { pkg  :: Ghc.PackageId
  , interfaceFile :: P.FilePath -- interface, i.e. .haddock file
  , htmlDir   :: P.FilePath -- root html source directory
  , exposed  :: Bool -- module exposure flag
  }
