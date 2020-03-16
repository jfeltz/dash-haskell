module PackageId where
import Distribution.Package
import Distribution.Version

unversioned :: PackageId -> PackageId
unversioned p = p { pkgVersion = nullVersion }

versionless :: String -> PackageId
versionless n = PackageIdentifier (mkPackageName n) nullVersion
