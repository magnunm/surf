{-# OPTIONS_GHC -Wall #-}
module Main where

import           Data.ByteString      (ByteString)
import           Data.CaseInsensitive (CI, original)
import           Data.Function        ((&))
import           Data.Text            (Text, empty, pack)
import           Data.Text.Encoding   (decodeUtf8)
import           Network.HTTP.Client  (CookieJar, responseHeaders,
                                       responseVersion)
import qualified Network.HTTP.Req     as Req
import           Rainbow              (Chunk, blue, bold, chunk, fore, grey,
                                       italic, putChunkLn, putChunksLn)
import           System.Environment   (getArgs)

import           InjectEnvVars        (injectEnvVars)
import           ParseRequestSpec     (parseSpec, splitIntoSpecifications)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Error: No request specifications file passed."
    else let filename = head args in
         runSpecsFromFile filename

-- | Run all the request specifications in the passed file. Each request
-- specification is separated by a comment line (starting with `#`).
-- Sub-strings in the format '${VAR}' are replaced by the value of the
-- environment variable 'VAR'
runSpecsFromFile :: String -> IO ()
runSpecsFromFile specsFileName = do
  fileContent <- readFile specsFileName
  fileContentWithInjectedEnv <- injectEnvVars fileContent
  case fileContentWithInjectedEnv of
    Left err -> print err
    Right content ->
      let specifications = splitIntoSpecifications content
      in runSpecs specifications Nothing

runSpecs :: [String] -> Maybe CookieJar -> IO ()
runSpecs [] _cookies = return ()
runSpecs (firstSpec:furtherSpecs) cookies =
  case parseSpec firstSpec cookies of
    Left err -> putStr (show err)
    Right request -> do
      printRequest firstSpec
      response <- Req.runReq httpConfig request
      printResponse response
      runSpecs furtherSpecs (Just (Req.responseCookieJar response))

-- | HTTP config to avoid raising exceptions on non-success status codes and
-- disable retries. In the context of this client we want the raw responses,
-- regardless of status code.
httpConfig :: Req.HttpConfig
httpConfig = Req.defaultHttpConfig
  { Req.httpConfigCheckResponse = \_ _ _ -> Nothing
  , Req.httpConfigRetryJudge = \_ _ -> False
  }

printRequest :: String -> IO ()
printRequest spec =
  putChunkLn $ chunk (pack (head (lines spec) ++ "\n")) & bold & fore blue

printResponse :: Req.BsResponse -> IO ()
printResponse response = putChunksLn $ showResponse response

showResponse :: Req.BsResponse -> [Chunk]
showResponse response =
  [
    (chunk . decodeUtf8 . Req.responseBody) response
  , "\n// "
    <> chunk (showHttpVersion response)
    <> " "
    <> chunk (showStatus response)
    <> "\n"
    <> chunk (showHeaders response)
    & fore grey & italic
  ]

showHttpVersion :: Req.BsResponse -> Text
showHttpVersion = pack . show . responseVersion . Req.toVanillaResponse

showStatus :: Req.BsResponse -> Text
showStatus response = (pack . show . Req.responseStatusCode) response
  <> " "
  <> (decodeUtf8 . Req.responseStatusMessage) response

showHeaders :: Req.BsResponse -> Text
showHeaders response = foldl (<>) empty (map showHeader (responseHeaders vanillaResponse))
  where vanillaResponse = Req.toVanillaResponse response

showHeader :: (CI ByteString, ByteString) -> Text
showHeader (name, value) = "// "
  <> decodeUtf8 (original name)
  <> ": "
  <> decodeUtf8 value
  <> "\n"
