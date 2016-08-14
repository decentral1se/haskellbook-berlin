{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Options

siteTitle = "Berlin haskellbook.com reading group 2016."

config = defaultConfiguration
    { deployCommand = "./deploy.sh"
    }

main :: IO ()
main = hakyllWith config $ do
    match "css/fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "index.markdown" $ do
        route $ setExtension "html"
        compile $ do
            let indexCtx = mconcat
                    [ field "toc" $ \item ->
                            loadBody ((itemIdentifier item) { identifierVersion = Just "toc"})
                    , defaultContext
                    ]

            pandocCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

        -- from http://julien.jhome.fr/posts/2013-05-14-adding-toc-to-posts.html
        version "toc" $
           compile $ pandocCompilerWith defaultHakyllReaderOptions
                                        defaultHakyllWriterOptions {
                                            writerTableOfContents = True
                                          , writerTemplate = "$toc$"
                                          , writerStandalone = True
                                          }

    match "templates/*" $
        compile templateCompiler
