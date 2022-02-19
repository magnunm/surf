module Main where

import Data.Aeson ((.=), object, Value)
import qualified Data.Aeson.Key as Key
import Data.Text (pack, Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T.IO
import qualified Network.HTTP.Req as Req

main :: IO ()
main = do
  filename <- getLine
  fileContent <- readFile filename
  let httpMethod = head (words fileContent)
  let rawUrl = head (tail (words fileContent))
  let convertedUrl = convertUrl rawUrl
  response <- Req.runReq Req.defaultHttpConfig (request httpMethod convertedUrl)
  T.IO.putStr (decodeUtf8 (Req.responseBody response))

request :: Req.MonadHttp m => String -> Req.Url scheme -> m Req.BsResponse
request httpMethod url
  | httpMethod == "GET" = get url
  | httpMethod == "POST" = postJSON url payload
  | otherwise = error "FIXME"

-- TODO: How do I type annotate this?
-- TODO: Support for specifying the scheme
convertUrl url
  | length (splitUrl url) < 2 = Req.https (pack url)
  | otherwise = foldl (Req./:) (hostAndScheme url) (tail (splitUrl url))

hostAndScheme "" = Req.https (pack "")
hostAndScheme url = Req.https (head (splitUrl url))

splitUrl :: String -> [Text]
splitUrl "" = []
splitUrl url = case length xs of
    0 -> [pack x]
    1 -> [pack x, pack ""] -- Ends in forward slash
    _ -> pack x : splitUrl (tail xs)
  where (x, xs) = break (== '/') url

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
