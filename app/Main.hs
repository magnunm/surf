module Main where

import qualified Control.Monad.IO.Class as IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
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
  T.IO.putStr (T.Encoding.decodeUtf8 (Req.responseBody response))

request :: Req.MonadHttp m => String -> Req.Url scheme -> m Req.BsResponse
request httpMethod url
  | httpMethod == "GET" = get url
  | httpMethod == "POST" = postJSON url payload
  | otherwise = error "FIXME"

-- TODO: How do I type annotate this?
-- TODO: Support for specifying the scheme
convertUrl url
  | length (splitUrl url) < 2 = Req.https (T.pack url)
  | otherwise = foldl (Req./:) (hostAndScheme url) (tail (splitUrl url))

hostAndScheme "" = Req.https (T.pack "")
hostAndScheme url = Req.https (head (splitUrl url))

splitUrl :: String -> [T.Text]
splitUrl "" = []
splitUrl url = case length xs of
    0 -> [T.pack x]
    1 -> [T.pack x, T.pack ""] -- Ends in forward slash
    _ -> T.pack x : splitUrl (tail xs)
  where (x, xs) = break (== '/') url

payload = Aeson.object
  [ Aeson.Key.fromString "foo" Aeson..= "bar"
  ]

postJSON
    :: Req.MonadHttp m
    => Req.Url scheme 
    -> Aeson.Value 
    -> m Req.BsResponse
postJSON url body = 
    Req.req Req.POST url (Req.ReqBodyJson body) Req.bsResponse mempty

get :: (Req.MonadHttp m) => Req.Url scheme -> m Req.BsResponse
get url =
    Req.req Req.GET url Req.NoReqBody Req.bsResponse mempty
