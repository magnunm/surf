{-# OPTIONS_GHC -Wall #-}
module Main where

import           Data.ByteString      (ByteString)
import           Data.CaseInsensitive (CI, original)
import           Data.Text            (Text, empty, pack)
import           Data.Text.Encoding   (decodeUtf8)
import qualified Data.Text.IO         as T.IO
import           Network.HTTP.Client  (CookieJar, responseHeaders,
                                       responseVersion)
import qualified Network.HTTP.Req     as Req
import           System.Environment   (getArgs)

import           ParseRequestSpec     (parseSpec, splitIntoSpecifications)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Error: No request specifications file passed."
    else let filename = head args in
         runSpecsFromFile filename

-- | Run all the request specifications in the passed file. Each request
-- specification is separated by a comment line (starting with `#`)
runSpecsFromFile :: String -> IO ()
runSpecsFromFile specsFileName = do
  fileContent <- readFile specsFileName
  let specifications = splitIntoSpecifications fileContent
  runSpecs specifications Nothing

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
printRequest spec = putStr $ head (lines spec) ++ "\n\n"

printResponse :: Req.BsResponse -> IO ()
printResponse response = T.IO.putStr $ showResponse response

showResponse :: Req.BsResponse -> Text
showResponse response = (decodeUtf8 . Req.responseBody) response
  <> pack "\n// " <> showHttpVersion response <> pack " " <> showStatus response
  <> pack "\n" <> showHeaders response
  <> pack "\n"

showHttpVersion :: Req.BsResponse -> Text
showHttpVersion = pack . show . responseVersion . Req.toVanillaResponse

showStatus :: Req.BsResponse -> Text
showStatus response = (pack . show . Req.responseStatusCode) response
  <> pack " "
  <> (decodeUtf8 . Req.responseStatusMessage) response

showHeaders :: Req.BsResponse -> Text
showHeaders response = foldl (<>) empty (map showHeader (responseHeaders vanillaResponse))
  where vanillaResponse = Req.toVanillaResponse response

showHeader :: (CI ByteString, ByteString) -> Text
showHeader (name, value) = pack "// "
  <> decodeUtf8 (original name)
  <> pack ": "
  <> decodeUtf8 value
  <> pack "\n"
