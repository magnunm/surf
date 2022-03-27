module ParseRequestSpec (parseSpecs, splitIntoSpecifications, SpecificationParseError) where

import Data.Monoid ((<>))
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
    Right (Left url) -> do
      headers <- addSpecHeaders specification mempty
      request httpMethod url body headers
    Right (Right url) -> do
      headers <- addSpecHeaders specification mempty
      request httpMethod url body headers

-- | Each line right after the method and URL is interpreted as specifying a
-- header. Add the headers to the request options.
addSpecHeaders
  :: String
  -> Req.Option scheme
  -> Either SpecificationParseError (Req.Option scheme)
addSpecHeaders specification options =
  if length (lines specification) < 2
    then Right mempty
    else
      let headerLines = takeWhile ("" /=) (tail (lines specification))
          addHeaderFuncs = map addColonSeparatedHeader headerLines
      in
        foldl (>>=) (Right options) addHeaderFuncs

-- | Add a header from two colon-separated strings to the options
addColonSeparatedHeader
  :: String
  -> Req.Option scheme
  -> Either SpecificationParseError (Req.Option scheme)
addColonSeparatedHeader line options =
  if length rest < 2
    then
      Left $ SpecificationParseError (
        "Invalid header, no value specified in '" ++ line ++ "'"
      )
    else
      let value = dropWhile (' ' ==) (tail rest)
          encodedValue = encodeUtf8 . pack $ value
          encodedName = encodeUtf8 . pack $ name
      in
      Right $ options <> Req.header encodedName encodedName
  where (name, rest) = break (':' ==) line

-- | Anything after the first blank line of the specification is interpreted as
-- the body.
getSpecBody :: String -> Text
getSpecBody specification =
  if length fromFirstBlankLine < 2
    -- No blank line or just a blank line: no body
    then pack ""
    -- Rest of spec interpreted as the request body, empty initial lines ignored
    else pack $ unlines $ dropWhile ("" ==) fromFirstBlankLine
  where fromFirstBlankLine = dropWhile ("" /=) (lines specification)

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
  -> Req.Option scheme
  -> Either SpecificationParseError (m Req.BsResponse)
request httpMethod url body headers
  | httpMethod == "GET" = Right (get url headers)
  | httpMethod == "POST" = Right (post url body headers)
  | otherwise = Left (SpecificationParseError "Unsupported HTTP method")

post
    :: Req.MonadHttp m
    => Req.Url scheme
    -> Text
    -> Req.Option scheme
    -> m Req.BsResponse
post url body =
    Req.req Req.POST url (Req.ReqBodyBs (encodeUtf8 body)) Req.bsResponse

get :: (Req.MonadHttp m) => Req.Url scheme -> Req.Option scheme -> m Req.BsResponse
get url =
    Req.req Req.GET url Req.NoReqBody Req.bsResponse

isComment :: String -> Bool
isComment [] = False
isComment line = head line == '#'

isCommentOrEmpty :: String -> Bool
isCommentOrEmpty [] = True
isCommentOrEmpty line = isComment line
