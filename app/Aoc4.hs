module Main where

import Data.List.Split (splitOneOf)

checkInRange :: String -> Bool
checkInRange x = bina || ainb
  where
    (a, b) = parseRange x

    bina = all (`elem` a) b
    ainb = all (`elem` b) a

checkOverlap :: String -> Bool
checkOverlap x = bina
  where
    (a, b) = parseRange x
    bina = any (`elem` a) b

parseRange :: String -> ([Int], [Int])
parseRange x = (a, b)
  where
    [ax, ay, bx, by] = splitOneOf "-," x

    a = [(read ax :: Int) .. (read ay :: Int)]
    b = [(read bx :: Int) .. (read by :: Int)]

aoc4part1 :: String -> Int
aoc4part1 file = count
  where
    rangeLines = filter (not . null) (lines file)

    inRange = filter checkInRange rangeLines

    count = length inRange

aoc4part2 :: String -> Int
aoc4part2 file = count
  where
    rangeLines = filter (not . null) (lines file)

    inRange = filter checkOverlap rangeLines

    count = length inRange

main :: IO ()
main = do
  contents <- readFile "day4.txt"
  print $ aoc4part1 contents
  print $ aoc4part2 contents
