module Main where

import           Universum
import           Lib

main :: IO ()
main = do
  contents <- readFile "./resources/input.txt"
  let expenses = rights $ fmap readEither (lines contents)
  let result   = multipliedSum expenses 2020
  print result

