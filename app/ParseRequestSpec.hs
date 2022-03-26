module ParseRequestSpec (parseSpecs, splitIntoSpecifications, SpecificationParseError) where

import Data.Text (pack, Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Req as Req

import Url (convertUrl, UrlParseError)

newtype SpecificationParseError = SpecificationParseError String

instance Show SpecificationParseError where
  show (SpecificationParseError message) = "Error parsing request: " ++ message

-- | Parse multiple requests specifications, ending at the first error if any.
parseSpecs
  :: (Req.MonadHttp m)
  => [String]
  -> Either SpecificationParseError [m Req.BsResponse]
parseSpecs = mapM parseSpec

-- | Split a string containing multiple request specifications into a
-- list of strings, where each string is a single specification
-- with no comments.
splitIntoSpecifications :: String -> [String]
splitIntoSpecifications multipleSpecs
  | null firstSpec = []
  | null rest = [unlines firstSpec]
  | otherwise = unlines firstSpec : splitIntoSpecifications (unlines rest)
  where
    asLines = lines multipleSpecs
    (firstSpec, rest) = break isComment (dropWhile isCommentOrEmpty asLines)

-- | Parse a single request specification.
parseSpec
  :: (Req.MonadHttp m)
  => String
  -> Either SpecificationParseError (m Req.BsResponse)
parseSpec [] = Left (SpecificationParseError "Empty specification")
parseSpec specification = do
  let firstLine = head (lines specification)
  httpMethod <- getSpecMethod firstLine
  rawUrl <- getSpecRawUrl firstLine
  let body = getSpecBody specification
  case convertUrl rawUrl of
    Left error -> Left (SpecificationParseError (show error))
    Right (Left url) -> request httpMethod url body
    Right (Right url) -> request httpMethod url body

getSpecBody :: String -> Text
getSpecBody specification =
  if length specLines < 2
    -- Just first line with method and URL, no body
    then pack ""
    -- Rest of spec interpreted as the request body, empty initial lines ignored
    else pack $ unlines $ dropWhile ("" ==) (tail specLines)
  where specLines = lines specification

getSpecMethod :: String -> Either SpecificationParseError String
getSpecMethod line =
  let lineWords = words line in
    if null lineWords
      then Left (SpecificationParseError "Could not find method from empty line")
      else Right (head lineWords)

getSpecRawUrl :: String -> Either SpecificationParseError String
getSpecRawUrl line =
  let lineWords = words line in
    case lineWords of
      (method:rest) -> Right (head rest)
      _ -> Left (SpecificationParseError "Could not find URL from single word line, first word interpreted as method")

request
  :: Req.MonadHttp m
  => String
  -> Req.Url scheme
  -> Text
  -> Either SpecificationParseError (m Req.BsResponse)
request httpMethod url body
  | httpMethod == "GET" = Right (get url)
  | httpMethod == "POST" = Right (post url body)
  | otherwise = Left (SpecificationParseError "Unsupported HTTP method")

post
    :: Req.MonadHttp m
    => Req.Url scheme
    -> Text
    -> m Req.BsResponse
post url body =
    Req.req Req.POST url (Req.ReqBodyBs (encodeUtf8 body)) Req.bsResponse mempty

get :: (Req.MonadHttp m) => Req.Url scheme -> m Req.BsResponse
get url =
    Req.req Req.GET url Req.NoReqBody Req.bsResponse mempty

isComment :: String -> Bool
isComment [] = False
isComment line = head line == '#'

isCommentOrEmpty :: String -> Bool
isCommentOrEmpty [] = True
isCommentOrEmpty line = isComment line
