module Main where

import System.Environment (getArgs)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T.IO
import qualified Network.HTTP.Req as Req
import Control.Monad.IO.Class (MonadIO)

import Url (convertUrl, UrlParseError)
import ParseRequestSpec (parseSpecs, splitIntoSpecifications)

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
  runSpecs specifications

runSpecs :: [String] -> IO ()
runSpecs specifications =
  case parseSpecs specifications of
    Left error -> putStr (show error)
    Right requests -> runRequests requests >>= printResponses

runRequests :: MonadIO m => [Req.Req a] -> m [a]
runRequests = mapM runRequest

runRequest :: MonadIO m => Req.Req a -> m a
runRequest = Req.runReq Req.defaultHttpConfig

printResponses :: [Req.BsResponse] -> IO ()
printResponses responses = head <$> mapM printResponse responses

printResponse :: Req.BsResponse -> IO ()
printResponse = T.IO.putStr . decodeUtf8 . Req.responseBody
