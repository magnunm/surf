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

newtype UrlParseError = UrlParseError String
  deriving Show

convertUrl
  :: String
     -> Either
          UrlParseError
          (Either (Req.Url 'Http) (Req.Url 'Https))
convertUrl url = do
  untilPath <- hostAndScheme url
  hostAndPath <- case fromSequence "://" url of
    Just hostAndPath -> Right hostAndPath
    Nothing -> Left noSchemeSeparatorError
  let hostAndPathSegments = splitPath hostAndPath
  if length hostAndPathSegments < 2
    then Right untilPath
    else case untilPath of
      Left url -> Right (Left (foldl (/:) url (tail hostAndPathSegments)))
      Right url -> Right (Right (foldl (/:) url (tail hostAndPathSegments)))

splitPath :: String -> [Text]
splitPath "" = []
splitPath url = case length fromSlash of
    0 -> [pack untilSlash]
    1 -> [pack untilSlash, pack ""] -- Ends in forward slash
    _ -> pack untilSlash : splitPath (tail fromSlash)
  where (untilSlash, fromSlash) = break (== '/') url

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
untilSequence sequence original = if null matchingInits
  then Nothing
  else Just (take
              (length (head matchingInits) - length sequence)
              (head matchingInits))
  where matchingInits = filter (isSuffixOf sequence) (inits original)

fromSequence :: (Eq a) => [a] -> [a] -> Maybe [a]
fromSequence sequence original = if null matchingTails
  then Nothing
  else Just (drop (length sequence) (head matchingTails))
  where matchingTails = filter (isPrefixOf sequence) (tails original)
