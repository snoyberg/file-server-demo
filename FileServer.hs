#!/usr/bin/env stack
{- stack
    --resolver lts-6.11
    --install-ghc
    runghc
    --package shakespeare
    --package wai-app-static
    --package wai-extra
    --package warp
 -}

-- The code above is used for Haskell Stack's script interpreter
-- feature. For more information, see:
-- https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter
--
-- Note how we explicitly list an LTS Haskell snapshot
-- (https://www.stackage.org/lts-6.11) to ensure reproducibility. We
-- then state which packages need to be present to run this code.

-- Enable the OverloadedStrings extension, a commonly used feature.
{-# LANGUAGE OverloadedStrings #-}

-- We use the QuasiQuotes to embed Hamlet HTML templates inside
-- our source file.
{-# LANGUAGE QuasiQuotes #-}

-- Import the various modules that we'll use in our code.
import qualified Data.ByteString.Char8          as S8
import qualified Data.ByteString.Lazy           as L
import           Data.Functor.Identity
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Parse
import           System.Environment
import           System.FilePath
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Hamlet

-- | Entrypoint to our application
main :: IO ()
main = do
    -- For ease of setup, we want to have a "sanity" command line
    -- argument. We'll see how this is used in the Dockerfile
    -- later. Desired behavior:
    --
    -- * If we have the argument "sanity", immediately exit
    -- * If we have no arguments, run the server
    -- * Otherwise, error out
    args <- getArgs
    case args of
        ["sanity"] -> putStrLn "Sanity check passed, ready to roll!"
        [] -> do
            putStrLn "Launching application"
            -- Run our application (defined below) on port 8080
            run 8080 app
        _ -> error $ "Unknown arguments: " ++ show args

-- | Our main application
app :: Application
app req send =
    -- Route the request based on the path requested
    case pathInfo req of
        -- "/": send the HTML homepage contents
        [] -> send $ responseBuilder
                status200
                [("Content-Type", "text/html; charset=utf-8")]
                (renderHtmlBuilder homepage)

        -- "/browse/...": use the file server to allow directory
        -- listings and downloading files
        ("browse":rest) ->
            -- We create a modified request that strips off the
            -- "browse" component of the path, so that the file server
            -- does not need to look inside a /browse/ directory
            let req' = req { pathInfo = rest }
             in fileServer req' send

        -- "/upload": handle a file upload
        ["upload"] -> upload req send

        -- anything else: 404
        _ -> send $ responseLBS
            status404
            [("Content-Type", "text/plain; charset=utf-8")]
            "Not found"

-- | Create an HTML page which links to the /browse URL, and allows
-- for a file upload
homepage :: Html
homepage = [shamlet|
$doctype 5
<html>
    <head>
        <title>File server
    <body>
        <h1>File server
        <p>
            <a href=/browse/>Browse available files

        <form method=POST action=/upload enctype=multipart/form-data>
            <p>Upload a new file
            <input type=file name=file>
            <input type=submit>
|]

-- | Use the standard file server settings to serve files from the
-- current directory
fileServer :: Application
fileServer = staticApp (defaultFileServerSettings ".")

-- | Handle file uploads, storing the file in the current directory
upload :: Application
upload req send = do
    -- Parse the request body. We'll ignore parameters and just look
    -- at the files
    (_params, files) <- parseRequestBody lbsBackEnd req

    -- Look for the file parameter called "file"
    case lookup "file" files of
        -- Not found, so return a 400 response
        Nothing -> send $ responseLBS
            status400
            [("Content-Type", "text/plain; charset=utf-8")]
            "No file parameter found"
        -- Got it!
        Just file -> do
            let
                -- Determine the name of the file to write out
                name = takeFileName $ S8.unpack $ fileName file
                -- and grab the content
                content = fileContent file
            -- Write it out
            L.writeFile name content

            -- Send a 303 response to redirect back to the homepage
            send $ responseLBS
                status303
                [ ("Content-Type", "text/plain: charset=utf-8")
                , ("Location", "/")
                ]
                "Upload successful!"
