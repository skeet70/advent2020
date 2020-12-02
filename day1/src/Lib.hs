{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Lib
  ( findSum
  , multipliedSum
  )
where

import           Lib.Prelude

-- | Finds two numbers in the provided list that sum to the provided target.
--
-- >>> findSum [1721,979,366,299,675,1456]
-- (1721, 299)
findSum :: [Int] -> Int -> Maybe (Int, Int)
findSum [] _ = Nothing
findSum (n : ns) target =
  if elem (target - n) ns then Just (n, target - n) else findSum ns target

multipliedSum :: [Int] -> Int -> Maybe (Int, Int, Int)
multipliedSum ns target = map (\(a, b) -> (a, b, a * b)) (findSum ns target)
