module Main where

import System.Environment (getArgs)
import Data.List (inits, tails, isPrefixOf, isSuffixOf)
import Data.Either (isLeft, fromRight)
import Data.Aeson ((.=), object, Value)
import qualified Data.Aeson.Key as Key
import Data.Text (pack, Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T.IO
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req ((/:), Scheme( Https, Http ))

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
    Right (Left x) -> do response <- Req.runReq Req.defaultHttpConfig (
                          request httpMethod x)
                         T.IO.putStr (decodeUtf8 (Req.responseBody response))
    Right (Right x) -> do response <- Req.runReq Req.defaultHttpConfig (
                            request httpMethod x)
                          T.IO.putStr (decodeUtf8 (Req.responseBody response))

request :: Req.MonadHttp m => String -> Req.Url scheme -> m Req.BsResponse
request httpMethod url
  | httpMethod == "GET" = get url
  | httpMethod == "POST" = postJSON url payload
  | otherwise = error "FIXME"

newtype UrlParseError = UrlParseError String
  deriving Show

convertUrl
  :: String
     -> Either
          UrlParseError
          (Either (Req.Url 'Http) (Req.Url 'Https))
convertUrl url = do
  untilPath <- hostAndScheme url
  hostAndPath <- (case fromSequence "://" url of
    Just x -> Right x
    Nothing -> Left (UrlParseError "Could not find `://` in URL"))
  let hostAndPathSegments = splitPath hostAndPath
  if length hostAndPathSegments < 2
    then return untilPath
    else case untilPath of
      Left x -> return (Left (foldl (/:) x (tail hostAndPathSegments)))
      Right x -> return (Right (foldl (/:) x (tail hostAndPathSegments)))

splitPath :: String -> [Text]
splitPath "" = []
splitPath url = case length xs of
    0 -> [pack x]
    1 -> [pack x, pack ""] -- Ends in forward slash
    _ -> pack x : splitPath (tail xs)
  where (x, xs) = break (== '/') url

hostAndScheme
  :: String
     -> Either
          UrlParseError
          (Either (Req.Url 'Http) (Req.Url 'Https))
hostAndScheme url = do
  host <- getHost url
  case untilSequence "://" url of
    Just "http" -> Right (Left (Req.http host))
    Just "https" -> Right (Right (Req.https host))
    Just scheme -> Left (UrlParseError ("Unsupported scheme: " ++ scheme))
    Nothing -> Left noSchemeSeparatorError

getHost :: String -> Either UrlParseError Text
getHost url = case fromSequence "://" url of
  Just hostAndPath -> Right (head (splitPath hostAndPath))
  Nothing -> Left noSchemeSeparatorError

noSchemeSeparatorError = UrlParseError "Could not find `://` in URL"

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

untilSequence :: (Eq a) => [a] -> [a] -> Maybe [a]
untilSequence sequence original = if null x
  then Nothing
  else Just (take (length (head x) - length sequence) (head x))
  where x = filter (isSuffixOf sequence) (inits original)

fromSequence :: (Eq a) => [a] -> [a] -> Maybe [a]
fromSequence sequence original = if null x
  then Nothing
  else Just (drop (length sequence) (head x))
  where x = filter (isPrefixOf sequence) (tails original)
