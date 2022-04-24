{-# OPTIONS_GHC -Wall #-}
module Main where

import System.Environment (getArgs)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T.IO
import qualified Network.HTTP.Req as Req
import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Client (CookieJar)

import ParseRequestSpec (parseSpec, splitIntoSpecifications)

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
runSpecs specifications cookies =
  case parseSpec (head specifications) cookies of
    Left err -> putStr (show err)
    Right request -> do
      response' <- runRequest request
      let (newCookies, response) = extractCookieJar response'
      printResponse response
      putStr "\n"
      runSpecs (tail specifications) (Just newCookies)

runRequest :: MonadIO m => Req.Req a -> m a
runRequest = Req.runReq Req.defaultHttpConfig

extractCookieJar :: Req.BsResponse -> (CookieJar, Req.BsResponse)
extractCookieJar response = (Req.responseCookieJar response, response)

printResponse :: Req.BsResponse -> IO ()
printResponse = T.IO.putStr . decodeUtf8 . Req.responseBody
