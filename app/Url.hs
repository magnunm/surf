{-# OPTIONS_GHC -Wall #-}
module Url (UrlParseError, convertUrl) where

import           Data.List        (inits, isPrefixOf, isSuffixOf, tails)
import           Data.Text        (Text, pack)
import           Network.HTTP.Req (Scheme (Http, Https), (/:))
import qualified Network.HTTP.Req as Req

newtype UrlParseError = UrlParseError String

instance Show UrlParseError where
  show (UrlParseError message) = "Could not parse URL: " ++ message

-- | Convert a string URL to Request's Url data type
convertUrl
  :: String
     -> Either
          UrlParseError
          (Either (Req.Url 'Http) (Req.Url 'Https))
convertUrl url = do
  untilPath <- hostAndScheme url
  hostAndPath <- case fromSubList "://" url of
    Just hostAndPath -> Right hostAndPath
    Nothing          -> Left noSchemeSeparatorError
  let hostAndPathSegments = splitPath hostAndPath
  if length hostAndPathSegments < 2
    then Right untilPath -- No path
    else
      Right $ (`appendPathSegements` tail hostAndPathSegments) <$> untilPath

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
