{-# OPTIONS_GHC -Wall #-}
module Main where

import           Data.Text.Encoding  (decodeUtf8)
import qualified Data.Text.IO        as T.IO
import           Network.HTTP.Client (CookieJar)
import qualified Network.HTTP.Req    as Req
import           System.Environment  (getArgs)

import           ParseRequestSpec    (parseSpec, splitIntoSpecifications)

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

-- | HTTP config to avoid raising exceptions on non-success status codes and
-- disable retries. In the context of this client we want the raw responses,
-- regardless of status code.
httpConfig :: Req.HttpConfig
httpConfig = Req.defaultHttpConfig
  { Req.httpConfigCheckResponse = \_ _ _ -> Nothing
  , Req.httpConfigRetryJudge = \_ _ -> False
  }

runSpecs :: [String] -> Maybe CookieJar -> IO ()
runSpecs [] _cookies = return ()
runSpecs specifications cookies =
  case parseSpec (head specifications) cookies of
    Left err -> putStr (show err)
    Right request -> do
      response <- Req.runReq Req.defaultHttpConfig request
      printResponse response
      runSpecs (tail specifications) (Just (Req.responseCookieJar response))

printResponse :: Req.BsResponse -> IO ()
printResponse response = do
  (T.IO.putStr . decodeUtf8 . Req.responseBody) response
  putStr "\n"
