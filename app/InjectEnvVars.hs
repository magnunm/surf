{-# OPTIONS_GHC -Wall #-}
module InjectEnvVars where

import           System.Environment (lookupEnv)

newtype InjectEnvVarError = InjectEnvVarError String

instance Show InjectEnvVarError where
  show (InjectEnvVarError message) = "Error injecting env var: " ++ message

type Result = IO (Either InjectEnvVarError String)

injectEnvVars :: String -> Result
injectEnvVars str = if null fromFirstEnvVar
  then return (Right str)
  else fmap (untilFirstEnvVar ++) <$> replaceStartWithEnvVar fromFirstEnvVar
  where (untilFirstEnvVar, fromFirstEnvVar) = splitAtEnvVarStart str

replaceStartWithEnvVar :: String -> Result
replaceStartWithEnvVar str = if null fromEndBracket
  then return (Left (InjectEnvVarError "No ending } found for env var"))
  else let envVarValue = getEnv' firstEnvVar
           afterEnvVar = injectEnvVars (tail fromEndBracket)
       in
           combineUsing (combineUsing (++)) envVarValue afterEnvVar
  where (firstEnvVar, fromEndBracket) = break (== '}') str

getEnv' :: String -> Result
getEnv' name = mapMaybe <$> lookupEnv name
  where mapMaybe x = case x of
          Just y -> Right y
          _      -> Left (InjectEnvVarError "Env var not found")

splitAtEnvVarStart :: String -> (String, String)
splitAtEnvVarStart "" = ("", "")
splitAtEnvVarStart str = if length fromDollar > 2
  then if head (tail fromDollar) == '{'
       then (untilDollar, tail (tail fromDollar))
       else let (rest, fromEnvVarStart) = splitAtEnvVarStart (tail fromDollar) in
         (untilDollar ++ "$" ++ rest, fromEnvVarStart)
  else (str, "")
  where (untilDollar, fromDollar) = break (== '$') str

combineUsing :: Monad m => (a -> b -> c) -> m a -> m b -> m c
combineUsing f x y = do
  xInner <- x
  f xInner <$> y
