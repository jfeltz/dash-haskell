module PackageId where
import Distribution.Package
import Distribution.Version

emptyVersion :: Version
emptyVersion = Version [] []
              
unversioned :: PackageId -> PackageId
unversioned p = p { pkgVersion = emptyVersion } 

versionless :: String -> PackageId
versionless n = PackageIdentifier (PackageName n) emptyVersion 
