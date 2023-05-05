{-# OPTIONS_GHC -Wall #-}
module Main where

import           System.Environment (getArgs)

import           Lib

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Error: No request specifications file passed."
    else let filename = head args in
         runSpecsFromFile filename
