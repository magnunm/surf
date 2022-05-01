{-# OPTIONS_GHC -Wall #-}
module Url (UrlParseError, convertUrl) where

import           Data.List        (inits, isPrefixOf, isSuffixOf, tails)
import           Data.Text        (Text, pack)
import           Network.HTTP.Req (Scheme (Http, Https), (/:))
import qualified Network.HTTP.Req as Req

newtype UrlParseError = UrlParseError String

instance Show UrlParseError where
  show (UrlParseError message) = "Could not parse URL: " ++ message

type ParsedUrl = Either (Req.Url 'Http, Req.Option 'Http) (Req.Url 'Https, Req.Option 'Https)

-- | Convert a string URL to Request's Url data type together with Options
-- containing the URL query parameters (if any).
convertUrl :: String -> Either UrlParseError ParsedUrl
convertUrl rawUrl = do
  untilPath <- hostAndScheme rawUrl
  hostPathAndQueryParams <- case fromSubList "://" rawUrl of
    Just hostPathAndQueryParams -> Right hostPathAndQueryParams
    Nothing                     -> Left noSchemeSeparatorError
  let (hostAndPath, rawQueryParams) = break (== '?') hostPathAndQueryParams
  let queryParams = specQueryParams rawQueryParams
  let hostAndPathSegments = splitPath hostAndPath
  let untilQueryParams = if length hostAndPathSegments < 2
                         then Right untilPath -- No path
                         else Right $ (`appendPathSegements` tail hostAndPathSegments) <$> untilPath
  case untilQueryParams of
    Right (Left url)  -> Right (Left (url, queryParams))
    Right (Right url) -> Right (Right (url, queryParams))
    Left err          -> Left err

specQueryParams :: String -> Req.Option scheme
specQueryParams [] = mempty
specQueryParams rawQueryParams =
  let
    sanitizedInput = dropWhile (\x -> (x == '?') || (x == '&')) rawQueryParams
    (rawFirstQueryParam, rest) = break (== '&') sanitizedInput
    (name, valueAndEquals) = break (== '=') rawFirstQueryParam
    value = if length valueAndEquals < 2 then Nothing else Just (pack (tail valueAndEquals))
    firstQueryParam = Req.queryParam (pack name) value
  in
    firstQueryParam <> specQueryParams rest

appendPathSegements :: Req.Url scheme -> [Text] -> Req.Url scheme
appendPathSegements = foldl (/:)

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
