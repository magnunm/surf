module Main where

import qualified Control.Monad.IO.Class as IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Text as T
import qualified Network.HTTP.Req as Req

main :: IO ()
main = Req.runReq Req.defaultHttpConfig $ do
  let payload =
        Aeson.object
          [ Aeson.Key.fromString "foo" Aeson..= (10 :: Int),
            Aeson.Key.fromString "bar" Aeson..= (20 :: Int)
          ]
  r <-
    postJSON
      (Req.https (T.pack "httpbin.org") Req./: (T.pack "post"))
      payload
  IO.Class.liftIO (print (Req.responseBody r :: Aeson.Value))

postJSON
    :: (Req.MonadHttp m, Aeson.FromJSON a)
    => Req.Url scheme 
    -> Aeson.Value 
    -> m (Req.JsonResponse a)
postJSON url body = 
    Req.req Req.POST url (Req.ReqBodyJson body) Req.jsonResponse mempty

get :: (Req.MonadHttp m) => Req.Url scheme -> m Req.BsResponse
get url =
    Req.req Req.GET url Req.NoReqBody Req.bsResponse mempty
