module Main where

import Data.List
import Data.List.Split

groupA :: String -> [String]
groupA = splitOn "\n\n"

groupSum :: String -> Int
groupSum s =
  sum $ map read (lines s)

groupSums :: String -> [Int]
groupSums s =
  map groupSum (groupA s)

aoc1 :: String -> Int
aoc1 s =
  maximum $ groupSums s

aoc11 :: String -> Int
aoc11 s =
  sum $ take 3 $ reverse $ sort $ groupSums s

main :: IO ()
main = do
  contents <- readFile "day1.txt"
  print (aoc1 contents)
  print (aoc11 contents)
