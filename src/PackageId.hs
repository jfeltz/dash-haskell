module PackageId where
import qualified Distribution.Package as C
import Distribution.Version

emptyVersion :: Version
emptyVersion = Version [] []
              
unversioned :: C.PackageId -> C.PackageId
unversioned p = p { C.pkgVersion = emptyVersion } 

versionless :: String -> C.PackageId
versionless n = C.PackageIdentifier (C.PackageName n) emptyVersion 
