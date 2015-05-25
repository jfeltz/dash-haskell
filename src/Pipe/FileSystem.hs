-- TODO repl test file path methods.

{-# LANGUAGE OverloadedStrings #-}
module Pipe.FileSystem where
import           Control.Monad
import           Control.Monad.M
import qualified Data.ByteString as BS
import           Data.ByteString.Char8 (unpack, pack)
import qualified Data.List as L
import           Data.String.Util
import qualified Data.Text as T
import           Data.Text.Encoding
import           Database.SQLite.Simple
import           System.FilePath
import           PackageConf
import           Pipes
import           System.Directory ( doesDirectoryExist, getDirectoryContents )
import qualified System.Directory as D
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Haddock.Artifact
import           Haddock.Sqlite
import qualified Module as Ghc

-- TODO the utility of some of these fields is still unclear to me,
-- at the moment they are filled simply to satisfy the docset spec.
plist :: String -> BS.ByteString
plist str = pack . unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"
  , "<plist version=\"1.0\">"
  , "<dict>"
  , "<key>CFBundleIdentifier</key>"
  , "<string>" ++ str ++ "</string>"
  , "<key>CFBundleName</key>"
  , "<string>docset for Haskell package " ++ str ++ "</string>"
  , "<key>DocSetPlatformFamily</key>"
  , "<string>haskell</string>" 
  , "<key>isDashDocset</key>"
  , "<true/>"
  , "<key>dashIndexFilePath</key>"
  , "<string>index.html</string>"
  , "</dict>"
  , "</plist>"
  ]

docsetDir :: Ghc.PackageKey -> FilePath
docsetDir k = Ghc.packageKeyString k ++ ".docset" 

leafs :: (FilePath -> Bool) -> FilePath -> ProducerM FilePath ()
leafs incPred p = do
  names <- liftIO $ getDirectoryContents p
  forM_ (filter (`notElem` [".", ".."]) names) $ \name' -> do
    let path = p </> name'
    dir <- liftIO . doesDirectoryExist $ path
    (if dir then 
      leafs incPred 
     else if not . incPred $ path then const (return ()) else yield)
      path

type Tag' = Tag T.Text 

remoteUrl :: T.Text -> Bool 
remoteUrl url = any (T.isPrefixOf url) ["http://", "https://"]

-- toStripped :: FilePath -> FilePath -> Either String FilePath
-- toStripped pfx original =
         
common :: FilePath -> FilePath -> FilePath
common l r = fst . unzip . takeWhile (uncurry (==)) $ zip (normalise l) (normalise r)

parent :: FilePath -> FilePath
parent = takeDirectory  
       
stripPrefix :: FilePath -> FilePath -> Either String FilePath 
stripPrefix prefix path = 
  if L.isPrefixOf prefix path then
    Right $ L.drop (L.length prefix + 1) path
  else 
    Left $ "prefix: " ++ prefix ++ " doesn't agree with:\n  " ++ path

toRelativePath :: FilePath -> FilePath -> Either String FilePath
toRelativePath base path = do
  let sharedPfx = common base path
  relative <- relativePath sharedPfx
  (</>) relative <$> stripPrefix sharedPfx path 
  where 
    relativePath :: FilePath -> Either String FilePath
    relativePath pfx = 
      joinPath 
       . flip replicate ".." 
       . length 
       . splitPath <$> stripPrefix pfx base 

relativize :: Ghc.PackageKey -> FilePath -> Either String T.Text 
relativize package p = 
  let filename'      = takeFileName p
      packageSubpath = Ghc.packageKeyString package
      matches        = 
        filter (packageSubpath ==) . reverse $ splitPath (parent p)
  in 
    T.pack <$> 
      if L.null matches then 
        return p -- preserve the path so that it still can be used 
      else -- assume as a package doc file and make relative
        toRelativePath packageSubpath $ L.head matches </> filename'

convertUrl ::  Ghc.PackageKey -> T.Text -> Either String T.Text 
convertUrl p urlExp 
  | T.null urlExp = Right T.empty
  | otherwise     =  
    if T.isPrefixOf "file:///" urlExp then 
      relativize p (T.unpack . T.drop 7 $ urlExp)
    else if T.isPrefixOf "/" urlExp then 
      relativize p $ T.unpack urlExp
    else
      Right urlExp
      
attributes :: FilePath -> Tag T.Text -> Either String [Attribute T.Text] 
attributes _ (TagOpen _ list) = 
  Right list
attributes src other            =    
  Left $
    "failed to retrieve expected attributes from tag:\n "
    ++ show other  ++ "\n in: \n" ++ src 
    
-- | Convert local package-compiled haddock links to local relative. 
convertLink :: Ghc.PackageKey -> FilePath -> Tag' -> Either String Tag'
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

pipe_htmlConvert :: 
  Ghc.PackageKey -> PipeM FilePath (FilePath, Maybe BS.ByteString) ()
pipe_htmlConvert p = 
  forever $ do
    src <- await
    if Just (takeExtension src) /= Just "html" 
      then  
        yield (src, Nothing)
      else do 
        buf <- T.pack <$> liftIO (readFile src)
        -- Link conversion errors are non-fatal.
        case mapM (convertLink p src) . parseTags $ buf of
          Left e -> do 
            lift . warning $ 
              preposition "failed to convert links" "for" "file" src [e]
            yield (src, Nothing) 
          Right tags ->
            yield (src, Just . encodeUtf8 . renderTags $ tags) 

-- | This consumes a doc file and copies it to a path in 'dstRoot'. 
-- By pre-condition: 
--   path has src_root as an ancestor 
-- By post-condition: 
--   written dst is the difference of path and src_root,
--   with by the concatenation of dst_root as it's parent. 
cons_writeFile :: 
  FilePath -> FilePath -> ConsumerM (FilePath, Maybe BS.ByteString) () 
cons_writeFile src_root dst_root = forever $ do 
  (path, buf) <- await
  dst_relative_path <- lift . fromE $ stripPrefix src_root path 

  -- liftIO . putStrLn $ "src_root: " ++ show src_root
  -- liftIO . putStrLn $ "relative path: " ++ show dst_relative_path
  -- liftIO . putStrLn $ "path: " ++ show path

  liftIO $ do 
    let dst_path = dst_root </> dst_relative_path
    --liftIO . putStrLn $ "dst path: " ++ dst_path
    -- create requisite parent directories for the file at the destination
    D.createDirectoryIfMissing True $ parent dst_path 
    case buf of 
      Nothing   -> D.copyFile path dst_path 
      Just buf' -> writeFile dst_path $ unpack buf'
  
cons_writeFiles :: FilePath -> ConsumerM Conf ()
cons_writeFiles docsets_root = forever $ do
  conf <- await
  
  lift . msg $ "processing: " ++ (Ghc.packageKeyString . pkg $ conf)
  let docset_folder = docsetDir (pkg conf) 
      dst_root      = docsets_root </> docset_folder 
      dst_doc_root  = dst_root </> "Contents/Resources/Documents/"

  liftIO . D.createDirectoryIfMissing True $ dst_doc_root 
 
  -- Copy all files and convert if necessary 
  lift . indentM 2 $ msg "writing files.."
  
  lift . runEffect $ 
    cons_writeFile (htmlDir conf) dst_doc_root 
    <-< pipe_htmlConvert (pkg conf)
    <-< leafs (\p -> Just (takeExtension p) /= Just "haddock") (htmlDir conf)
  
  -- TODO Since the haddock index is already produced in source docs
  -- with latest packaging systems, this is likely unnecessary 
  -- liftIO $ do 
  --    putStrLn "running haddock indexes"
  --    runHaddockIndex (interfaceFile conf) dst_doc_root
  lift . indentM 2 $ msg "writing plist.."
  liftIO . writeFile (dst_root </> "Contents/Info.plist") . unpack . plist . 
    Ghc.packageKeyString . pkg $ conf 

  let db_path = dst_root </> "Contents/Resources/docSet.dsidx" 

  liftIO $ do
    db_exists <- D.doesFileExist db_path 
    when db_exists $ D.removeFile db_path

  -- Initialize the SQlite Db
  c' <- liftIO $ do 
        c <- open db_path 
        createTable c
        return c

  lift . indentM 2 $ msg "populating database.."

  -- Populate the SQlite Db 
  liftIO $ execute_ c' "BEGIN;"
  artifacts <- lift $ toArtifacts (pkg conf) (interfaceFile conf) 
  lift $ mapM_ (fromArtifact (pkg conf) c') artifacts
  liftIO $ execute_ c' "COMMIT;"
  liftIO . close $ c'
  lift . indentM 2 $ msg "finished populating sqlite database.."
  lift $ msg "\n"
