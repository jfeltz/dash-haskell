{-# LANGUAGE OverloadedStrings #-}
module Pipe.FileSystem where
import           Control.Monad
import           Control.Monad.M
import qualified Data.ByteString as BS
import           Data.ByteString.Char8 (pack)
import qualified Data.List as L
import           Data.String.Util
import qualified Data.Text as T
import           Data.Text.Encoding
import           Database.SQLite.Simple
import           Filesystem as F
import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as P
import           Package.Conf
import           Pipes
import           System.Directory ( doesDirectoryExist, getDirectoryContents )
import qualified System.Directory as D
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Haddock.Artifact
import           Haddock.Sqlite
import           Distribution.Package
import           Distribution.Text

-- TODO the utility of some of these fields is still unclear to me,
-- at the moment they are filled simply to satisfy the docset spec.
plist :: PackageIdentifier -> BS.ByteString
plist p = Data.ByteString.Char8.pack . unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"
  , "<plist version=\"1.0\">"
  , "<dict>"
  , "<key>CFBundleIdentifier</key>"
  , "<string>" ++ display p ++ "</string>"
  , "<key>CFBundleName</key>"
  , "<string>docset for Haskell package " ++ display p ++ "</string>"
  , "<key>DocSetPlatformFamily</key>"
  , "<string>haskell</string>" 
  , "<key>isDashDocset</key>"
  , "<true/>"
  , "<key>dashIndexFilePath</key>"
  , "<string>index.html</string>"
  , "</dict>"
  , "</plist>"
  ]

docsetDir :: PackageIdentifier -> P.FilePath
docsetDir p = P.decodeString $ display p ++ ".docset" 

leafs :: (P.FilePath -> Bool) -> P.FilePath -> ProducerM P.FilePath ()
leafs incPred p = do
  names <- liftIO . getDirectoryContents $ P.encodeString p
  forM_ (filter (`notElem` [".", ".."]) names) $ \name' -> do
    let path = p </> P.decodeString name'
    dir <- liftIO . doesDirectoryExist $ P.encodeString path
    (if dir then 
      leafs incPred 
     else if not . incPred $ path then const (return ()) else yield)
      path

type Tag' = Tag T.Text 

remoteUrl :: T.Text -> Bool 
remoteUrl url = any (T.isPrefixOf url) ["http://", "https://"]

toStripped :: P.FilePath -> P.FilePath -> Either String P.FilePath
toStripped pfx original =
  -- I don't understand why System.FilePath.CurrentOS necessitates
  -- additional checking after the prefix has already been determined.
  case P.stripPrefix pfx original of 
    Nothing ->
      Left $
        "attempted to strip prefix: \n " 
        ++ P.encodeString pfx ++ " from: \n " ++ P.encodeString original
    Just remainder ->  
      Right remainder

toRelativePath :: P.FilePath -> P.FilePath -> Either String P.FilePath
toRelativePath base path = do
  let sharedPfx = P.commonPrefix [base, path]
  relative <- relativePath sharedPfx 
  (</>) relative <$> toStripped sharedPfx path 
  where 
    relativePath :: P.FilePath -> Either String P.FilePath
    relativePath pfx = 
      P.concat 
       . flip replicate ".." 
       . length 
       . P.splitDirectories <$> toStripped pfx base 

relativize :: PackageIdentifier -> P.FilePath -> Either String T.Text 
relativize package p = 
  let filename  = P.filename p
      packageSubpath = P.decodeString $ display package
      matches = filter (packageSubpath ==) . reverse $ P.splitDirectories (P.parent p)
  in 
    T.pack . P.encodeString <$> 
      if L.null matches then 
        return p -- preserve the path so that it still can be used 
      else -- assume as a package doc file and make relative
        toRelativePath packageSubpath $ L.head matches </> filename

convertUrl ::  PackageIdentifier -> T.Text -> Either String T.Text 
convertUrl p urlExp 
  | T.null urlExp = Right T.empty
  | otherwise     =  
    if T.isPrefixOf "file:///" urlExp then 
      relativize p (P.fromText . T.drop 7 $ urlExp)
    else if T.isPrefixOf "/" urlExp then 
      relativize p $ P.fromText urlExp
    else
      Right urlExp
      
attributes :: P.FilePath -> Tag T.Text -> Either String [Attribute T.Text] 
attributes _ (TagOpen _ list) = 
  Right list
attributes src other            =    
  Left $
    "failed to retrieve expected attributes from tag:\n "
    ++ show other  ++ "\n in: \n" ++ P.encodeString src 
    
-- | Convert local package-compiled haddock links to local relative. 
convertLink :: PackageIdentifier -> P.FilePath -> Tag' -> Either String Tag'
convertLink package src tag =
  -- We're only interested in processing links             
  if not $ tagOpenLit "a" (anyAttrNameLit "href") tag then  
    Right tag
  else do
    preserved <- filter (\(n,_) -> n /= "href") <$> attributes src tag 
    let url = fromAttrib "href" tag
    
    if remoteUrl url 
      then 
        Right tag -- ignore remote links
      else do
        url' <- convertUrl package url 
        Right . TagOpen "a" $ ("href", url') : preserved 

pipe_htmlConvert :: PackageIdentifier -> PipeM P.FilePath (P.FilePath, Maybe BS.ByteString) ()
pipe_htmlConvert p = 
  forever $ do
    src <- await
    if P.extension src /= Just "html" 
      then  
        yield (src, Nothing)
      else do 
        buf <- liftIO $ F.readTextFile src
        -- Link conversion errors are non-fatal.
        case mapM (convertLink p src) . parseTags $ buf of
          Left e -> do 
            lift . warning $ 
              preposition "failed to convert links" "for" "file" (P.encodeString src) [e]
            yield (src, Nothing) 
          Right tags ->
            yield (src, Just . encodeUtf8 . renderTags $ tags) 

-- | This consumes a doc file and copies it to a path in 'dstRoot'. 
-- By pre-condition: 
--   path has src_root as an ancestor 
-- By post-condition: 
--   written dst is the difference of path and src_root,
--   with by the concatenation of dst_root as it's parent. 
cons_writeFile :: P.FilePath -> P.FilePath -> ConsumerM (P.FilePath, Maybe BS.ByteString) () 
cons_writeFile src_root dst_root = forever $ do 
  (path, buf) <- await
  case P.stripPrefix src_root path of
    Nothing -> lift . err $ 
       "filepath error when attempting to find common prefix between src: \n" 
       ++ P.encodeString path ++ "\n and: \n" ++ P.encodeString src_root
    Just dst_relative_path ->
      -- Yes, this could be shorter, but I try not to unnecessarily obfuscate
      liftIO $ do 
        let dst_path = dst_root </> dst_relative_path
        -- create requisite parent directories for the file at the destination
        F.createTree $ P.parent dst_path 
        case buf of 
          Nothing   -> F.copyFile path dst_path 
          Just buf' -> F.writeFile dst_path buf'
  
cons_writeFiles :: P.FilePath -> ConsumerM Conf ()
cons_writeFiles docsets_root = forever $ do
  conf <- await
  
  lift . msg $ "processing: " ++ (display . pkg $ conf)
  let docset_folder = docsetDir (pkg conf) 
      dst_root      = docsets_root </> docset_folder 
      dst_doc_root  = dst_root </> P.decodeString "Contents/Resources/Documents/"

  liftIO . F.createTree $ dst_doc_root 
 
  -- Copy all files and convert if necessary 
  lift . indentM 2 $ msg "writing files.."
  
  lift . runEffect $ 
    cons_writeFile (htmlDir conf) dst_doc_root 
    <-< pipe_htmlConvert (pkg conf)
    <-< leafs (\p -> P.extension p /= Just "haddock") (htmlDir conf)
  
  -- TODO Since the haddock index is already produced in source docs
  -- with latest packaging systems, this is likely unnecessary 
  -- liftIO $ do 
  --    putStrLn "running haddock indexes"
  --    runHaddockIndex (interfaceFile conf) dst_doc_root

  lift . indentM 2 $ msg "writing plist.."

  liftIO . F.writeFile (dst_root </> "Contents/Info.plist") $ plist (pkg conf) 

  lift . indentM 2 $ msg "populating database.."

  let db_path = dst_root </> P.decodeString "Contents/Resources/docSet.dsidx" 

  liftIO $ do
    db_exists <- D.doesFileExist . P.encodeString $ db_path 
    when db_exists $ F.removeFile db_path

  -- Initialize the SQlite Db
  c' <- liftIO $ do 
        c <- open . P.encodeString $ db_path 
        createTable c
        return c

  -- Populate the SQlite Db 
  liftIO $ execute_ c' "BEGIN;"
  artifacts <- lift $ toArtifacts (pkg conf) (interfaceFile conf) 
  lift $ mapM_ (fromArtifact (pkg conf) c') artifacts
  liftIO $ execute_ c' "COMMIT;"
  liftIO . close $ c'
  lift . indentM 2 $ msg "finished populating sqlite database.."
  lift $ msg "\n"
