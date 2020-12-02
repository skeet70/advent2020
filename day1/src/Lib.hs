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
  , subsets
  )
where

import           Lib.Prelude

-- | Finds two numbers in the provided list that sum to the provided target.
--
-- >>> findSum 2 2020 [1721,979,366,299,675,1456]
-- [1721, 299]
findSumSubset :: Int -> [[Int]] -> [Int]
findSumSubset _ [] = []
findSumSubset target (n : ns) =
  if sum n == target then n else findSumSubset target ns

findSum :: Int -> Int -> [Int] -> [Int]
findSum subsetSize target ns = findSumSubset target (subsets subsetSize ns)

multipliedSum :: Int -> Int -> [Int] -> Int
multipliedSum subsetSize target ns = product $ findSum subsetSize target ns

-- | Create subsets of all num combinations in nums that are n elements each.
subsets :: Int -> [a] -> [[a]]
subsets 0 _  = [[]]
subsets _ [] = []
subsets n (x : xs) | l < n     = []
                   | otherwise = map (x :) (subsets (n - 1) xs) ++ subsets n xs
  where l = length (x : xs)
