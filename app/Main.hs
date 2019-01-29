{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import GHC.Generics

import Control.Lens
import Data.Aeson as A
import Data.Aeson.Lens
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Lens
import Data.Time
import Text.Pandoc
import Text.Pandoc.Highlighting
import Text.Pandoc.Shared hiding (substitute)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics (Generic)
import Slick

import qualified System.FSNotify as Notify
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import qualified System.Clipboard as Clipboard
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Application.Static as Static



main :: IO ()
main = do
  forkIO fileWatcher
  previewServer

fileWatcher :: IO ()
fileWatcher = Notify.withManager $ \mgr -> do
  Notify.watchTree mgr "site" (const True) (const runBuild)
  forever $ threadDelay 1000000

previewServer :: IO ()
previewServer = do
  putStrLn "Listening on http://localhost:9000"
  putStrLn "URL Copied to clipboard!"
  Clipboard.setClipboardString "http://localhost:9000/index.html"
  Warp.run 9000 $ Static.staticApp $ Static.defaultWebAppSettings "dist"

runBuild :: IO ()
runBuild = shakeArgs shakeOptions { shakeVerbosity = Chatty } $
  do
    want ["site"]
    postCache <- jsonCache' loadPost
    -- Require all the things we need to build the whole site
    "site" ~> need ["static", "posts", "tikz", "dist/index.html"]
    -- Require all static assets
    "static" ~> do
      staticFiles <- getDirectoryFiles "." ["site/js//*", "site/images//*"]
      requireCss
      need (("dist" </>) . dropDirectory1 <$> staticFiles)
    -- Rule for handling static assets, just copy them from source to dest
    ["dist/css//*", "dist/js//*", "dist/images//*"] |%> \out -> do
      copyFileChanged ("site" </> dropDirectory1 out) out
     -- Find and require every post to be built
    "posts" ~> requirePosts
    -- Find and require every tikz image be built
    "tikz" ~> requireTikz
    -- build the syntax highlighing css
    "dist/css/syntax.css" %> buildSyntaxCss
    -- build the main table of contents
    "dist/index.html" %> buildIndex postCache -- rule for actually building posts "dist/posts//*.html" %> buildPost postCache
    -- rule for actually building tikz images
    "dist/tikz//*.png" %> buildTikz

destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

srcToDest :: FilePath -> FilePath
srcToDest p = "dist" </> dropDirectory1 p

srcToUrl :: FilePath -> String
srcToUrl = ("/" ++) . dropDirectory1 . (-<.> ".html")

{- Post Index -}

data IndexInfo = IndexInfo
  { posts :: [Post]
  } deriving (Generic, Show)

instance FromJSON IndexInfo

instance ToJSON IndexInfo

buildIndex :: (PostFilePath -> Action Post) -> FilePath -> Action ()
buildIndex postCache out = do
  posts <- postNames >>= traverse (postCache . PostFilePath)
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' out indexHTML

{- CSS -}
codeStyle :: Style
codeStyle = haddock

requireCss :: Action ()
requireCss = do
  cssFiles <- getDirectoryFiles "." ["site/css//*"]
  need $ "dist/css/syntax.css":(fmap (\p -> srcToDest p) cssFiles)

buildSyntaxCss :: FilePath -> Action ()
buildSyntaxCss out =
  writeFile' out $ styleToCss codeStyle

{- Posts -}

data Post = Post
  { title :: String
  , author :: String
  , content :: String
  , url :: String
  , date :: String
  , image :: Maybe String
  } deriving (Generic, Eq, Ord, Show)

instance FromJSON Post
instance ToJSON Post

newtype PostFilePath = PostFilePath String
  deriving (Show, Eq, Hashable, Binary, NFData)

postNames :: Action [FilePath]
postNames = getDirectoryFiles "." ["site/posts//*.md"]

requirePosts :: Action ()
requirePosts = do
  pNames <- postNames
  need ((\p -> srcToDest p -<.> "html") <$> pNames)


renderPost :: Text -> Action Value
renderPost t =
  let mathExtensions =
        [ Ext_tex_math_dollars
        , Ext_tex_math_double_backslash
        , Ext_latex_macros]
      writerOptions = def
        { writerExtensions = foldr enableExtension pandocExtensions mathExtensions
        , writerHighlightStyle = Just codeStyle
        , writerHTMLMathMethod = MathJax ""
        }
      readerOptions = def { readerExtensions = pandocExtensions }
  in loadUsing (readMarkdown readerOptions) (writeHtml5String writerOptions) t

loadPost :: PostFilePath -> Action Post
loadPost (PostFilePath postPath) = do
  let srcPath = destToSrc postPath -<.> "md"
  postData <- readFile' srcPath >>= renderPost . T.pack
  let postURL = T.pack . srcToUrl $ postPath
      withURL = _Object . at "url" ?~ String postURL
      withSrc = _Object . at "srcPath" ?~ String (T.pack srcPath)
  convert . withSrc . withURL $ postData


buildPost :: (PostFilePath -> Action Post) -> FilePath -> Action ()
buildPost postCache out = do
  let srcPath = destToSrc out -<.> "md"
      postUrl = srcToUrl srcPath
  post <- postCache (PostFilePath srcPath)
  template <- compileTemplate' "site/templates/post.html"
  writeFile' out . T.unpack $ substitute template (toJSON post)

{- Tikz Image Generation -}
newtype TikzFilePath = TikzFilePath String
  deriving (Show, Eq, Hashable, Binary, NFData)

tikzNames :: Action [FilePath]
tikzNames = getDirectoryFiles "." ["site/tikz//*.tex"]

-- tikzDestToSrc :: FilePath -> FilePath
-- tikzDestToSrc p =
--   let (d,f) = splitFileName p
--       p' = (takeDirectory p) </> "tikz" </> f
--   in "dist" </> dropDirectory1 p'

-- tikzSrcToDest :: FilePath -> FilePath
-- tikzSrcToDest p =
--   let (d,f) = splitFileName p
--       p' = (takeDirectory d) </> "images" </> f
--   in "dist" </> dropDirectory1 p'

requireTikz :: Action ()
requireTikz = do
  tNames <- tikzNames
  need ((\t -> srcToDest t -<.> "png") <$> tNames)

buildTikz :: FilePath -> Action ()
buildTikz out = do
  let srcPath = destToSrc out -<.> "tex"
  command_ [Cwd $ takeDirectory srcPath, EchoStdout False] "pdflatex" ["-shell-escape", takeFileName srcPath]
  copyFile' (srcPath -<.> "png") out
