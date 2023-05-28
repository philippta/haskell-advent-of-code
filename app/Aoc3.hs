module Main where

import Data.Char
import Data.List
import Data.List.Split (chunksOf)
import Data.List.Unique (uniq)

splitMiddle :: String -> (String, String)
splitMiddle s = splitAt (length s `div` 2) s

findEqualChar :: String -> String -> Char
findEqualChar s1 s2
  | head s1 == head s2 = head s1
  | head s1 > head s2 = findEqualChar s1 (tail s2)
  | head s1 < head s2 = findEqualChar (tail s1) s2

prio :: Char -> Int
prio c
  | isAsciiLower c = (ord c - ord 'a') + 1
  | isAsciiUpper c = (ord c - ord 'A') + 27

concatLinesUniq :: [String] -> String
concatLinesUniq xs = sort $ concatMap (uniq . sort) xs

duplicateChar :: String -> Char
duplicateChar s
  | head s == s !! 1 && s !! 1 == s !! 2 = head s
  | otherwise = duplicateChar (tail s)

aoc3part1 :: String -> Int
aoc3part1 file = prioSum
  where
    items = lines file
    splitItems = map splitMiddle items

    sortItems (a, b) = (sort a, sort b)
    sortedItems = map sortItems splitItems

    equalChars = map (uncurry findEqualChar) sortedItems

    prioSum = sum $ map prio equalChars

aoc3part2 :: String -> Int
aoc3part2 file = prioSum
  where
    items = lines file
    chunks = chunksOf 3 items

    groupedLines = map concatLinesUniq chunks

    badges = map duplicateChar groupedLines

    prioSum = sum $ map prio badges

main :: IO ()
main = do
  contents <- readFile "day3.txt"
  print $ aoc3part1 contents

main2 :: IO ()
main2 = do
  contents <- readFile "day3.txt"
  print $ aoc3part2 contents
