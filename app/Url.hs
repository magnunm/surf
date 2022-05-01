{-# OPTIONS_GHC -Wall #-}
module Url (UrlParseError, convertUrl) where

import           Data.Bifunctor   (bimap)
import           Data.List        (inits, isPrefixOf, isSuffixOf, tails)
import           Data.Maybe       (fromJust)
import           Data.Text        (Text, pack)
import           Network.HTTP.Req (Scheme (Http, Https), (/:))
import qualified Network.HTTP.Req as Req

newtype UrlParseError = UrlParseError String

instance Show UrlParseError where
  show (UrlParseError message) = "Could not parse URL: " ++ message

type ParsedUrl =
  Either (Req.Url 'Http, Req.Option 'Http) (Req.Url 'Https, Req.Option 'Https)

-- | Convert a string URL to Request's Url data type together with an Option
-- containing the URL query parameters (if any).
convertUrl :: String -> Either UrlParseError ParsedUrl
convertUrl rawUrl = do
  hostAndScheme' <- hostAndScheme rawUrl
  -- Safe here as long as the host and scheme is extracted first
  let (path, rawQueryParams) = pathAndQueryParams rawUrl
  let pathSegments = splitPath path
  let addPathSegements = flip (foldl (/:)) pathSegments
  let untilQueryParams = bimap addPathSegements addPathSegements hostAndScheme'
  let queryParams' = queryParams rawQueryParams
  Right $ bimap (, queryParams') (, queryParams') untilQueryParams

-- | Extract path and query parameters. Assumes '://' is present in the raw URL.
pathAndQueryParams :: String -> (String, String)
pathAndQueryParams rawUrl =
  (dropWhile (/= '/') hostAndPath, rawQueryParams)
  where fromScheme = fromJust $ fromSubList "://" rawUrl
        (hostAndPath, rawQueryParams) = break (== '?') fromScheme

queryParams :: String -> Req.Option scheme
queryParams [] = mempty
queryParams rawQueryParams =
  let
    rawQueryParams' = dropWhile (\x -> (x == '?') || (x == '&')) rawQueryParams
    (rawFirstQueryParam, rest) = break (== '&') rawQueryParams'
    (name, valueAndEquals) = break (== '=') rawFirstQueryParam
    value = if length valueAndEquals < 2
            then Nothing
            else Just (pack (tail valueAndEquals))
    firstQueryParam = Req.queryParam (pack name) value
  in
    firstQueryParam <> queryParams rest

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
  case untilSubList "://" url of
    Just "http"  -> Right (Left (Req.http host))
    Just "https" -> Right (Right (Req.https host))
    Just scheme  -> Left (UrlParseError ("Unsupported scheme: " ++ scheme))
    Nothing      -> Left noSchemeSeparatorError

getHost :: String -> Either UrlParseError Text
getHost url = case fromSubList "://" url of
  Just hostAndPath -> Right (head (splitPath hostAndPath))
  Nothing          -> Left noSchemeSeparatorError

noSchemeSeparatorError :: UrlParseError
noSchemeSeparatorError = UrlParseError "Could not find `://` in URL"

untilSubList :: (Eq a) => [a] -> [a] -> Maybe [a]
untilSubList subList original = if null matchingInits
  then Nothing
  else Just (take
              (length (head matchingInits) - length subList)
              (head matchingInits))
  where matchingInits = filter (isSuffixOf subList) (inits original)

fromSubList :: (Eq a) => [a] -> [a] -> Maybe [a]
fromSubList subList original = if null matchingTails
  then Nothing
  else Just (drop (length subList) (head matchingTails))
  where matchingTails = filter (isPrefixOf subList) (tails original)
