module Main where

import           Universum
import           Lib

main :: IO ()
main = do
  contents <- readFile "./resources/input.txt"
  let expenses = rights $ fmap readEither (lines contents)
  let result   = multipliedSum 2 2020 expenses
  print result

