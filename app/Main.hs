module Main where

import System.Environment (getArgs)
import Data.Aeson ((.=), object, Value)
import qualified Data.Aeson.Key as Key
import Data.Text (pack, Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T.IO
import qualified Network.HTTP.Req as Req
import Control.Monad.IO.Class (MonadIO, liftIO)

import Url (convertUrl, UrlParseError)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Error: No request specifications file passed."
    else let filename = head args in
         parseSpecsFromFile filename

-- | Run all the request specifications in the passed file. Each request
-- specification is separated by a comment line (starting with `#`)
parseSpecsFromFile :: String -> IO ()
parseSpecsFromFile specsFileName = do
  fileContent <- readFile specsFileName
  let specifications = splitIntoSpecifications fileContent
  case parseSpecs specifications of
    Left error -> putStr (show error)
    Right requests -> runRequests requests >>= printResponses

runRequests :: MonadIO m => [Req.Req a] -> m [a]
runRequests = mapM runRequest

runRequest :: MonadIO m => Req.Req a -> m a
runRequest = Req.runReq Req.defaultHttpConfig

printResponses :: [Req.BsResponse] -> IO ()
printResponses responses = head <$> mapM printResponse responses

printResponse :: Req.BsResponse -> IO ()
printResponse = T.IO.putStr . decodeUtf8 . Req.responseBody

newtype SpecificationParseError = SpecificationParseError String

instance Show SpecificationParseError where
  show (SpecificationParseError message) = "Error parsing request: " ++ message

-- | Parse multiple requests specifications, ending at the first error if any.
parseSpecs
  :: (Req.MonadHttp m)
  => [String]
  -> Either SpecificationParseError [m Req.BsResponse]
parseSpecs = mapM parseSpec

-- | Parse a single request specification.
parseSpec
  :: (Req.MonadHttp m)
  => String
  -> Either SpecificationParseError (m Req.BsResponse)
parseSpec [] = Left (SpecificationParseError "Empty specification")
parseSpec specification = do
  -- TODO: Support for more than one line.
  let firstLine = head (lines specification)
  httpMethod <- getSpecMethod firstLine
  rawUrl <- getSpecRawUrl firstLine
  case convertUrl rawUrl of
    Left error -> Left (SpecificationParseError (show error))
    Right (Left url) -> request httpMethod url
    Right (Right url) -> request httpMethod url

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
  -> Either SpecificationParseError (m Req.BsResponse)
request httpMethod url
  | httpMethod == "GET" = Right (get url)
  | httpMethod == "POST" = Right (postJSON url payload)
  | otherwise = Left (SpecificationParseError "Unsupported HTTP method")

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

isComment :: String -> Bool
isComment [] = False
isComment line = head line == '#'

isCommentOrEmpty :: String -> Bool
isCommentOrEmpty [] = True
isCommentOrEmpty line = isComment line
