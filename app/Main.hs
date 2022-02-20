module Main where

import System.Environment (getArgs)
import Data.Aeson ((.=), object, Value)
import qualified Data.Aeson.Key as Key
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T.IO
import qualified Network.HTTP.Req as Req

import Url (convertUrl)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Error: No request specifications file passed."
    else let filename = head args in
         runSpec filename

runSpec :: String -> IO ()
runSpec specFileName = do
  fileContent <- readFile specFileName
  let httpMethod = head (words fileContent)
  let rawUrl = head (tail (words fileContent))
  case convertUrl rawUrl of
    Left error -> print error
    Right (Left url) -> do response <- Req.runReq Req.defaultHttpConfig (
                            request httpMethod url)
                           T.IO.putStr (decodeUtf8 (Req.responseBody response))
    Right (Right url) -> do response <- Req.runReq Req.defaultHttpConfig (
                              request httpMethod url)
                            T.IO.putStr (decodeUtf8 (Req.responseBody response))

request :: Req.MonadHttp m => String -> Req.Url scheme -> m Req.BsResponse
request httpMethod url
  | httpMethod == "GET" = get url
  | httpMethod == "POST" = postJSON url payload
  | otherwise = error "FIXME"

payload = object
  [ Key.fromString "foo" .= "bar"
  ]

postJSON
    :: Req.MonadHttp m
    => Req.Url scheme
    -> Value
    -> m Req.BsResponse
postJSON url body =
    Req.req Req.POST url (Req.ReqBodyJson body) Req.bsResponse mempty

get :: (Req.MonadHttp m) => Req.Url scheme -> m Req.BsResponse
get url =
    Req.req Req.GET url Req.NoReqBody Req.bsResponse mempty
