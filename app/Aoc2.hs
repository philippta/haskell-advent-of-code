module Main where

data Hand = Rock | Paper | Scissors deriving (Show, Eq)

data Outcome = Win | Loss | Draw deriving (Show, Eq)

hand :: Char -> Hand
hand 'A' = Rock
hand 'B' = Paper
hand 'C' = Scissors
hand 'X' = Rock
hand 'Y' = Paper
hand 'Z' = Scissors

outcome :: Char -> Outcome
outcome 'A' = Win
outcome 'B' = Draw
outcome 'C' = Loss
outcome 'X' = Win
outcome 'Y' = Draw
outcome 'Z' = Loss

play :: Hand -> Hand -> Outcome
play Rock Paper = Loss
play Rock Scissors = Win
play Paper Rock = Win
play Paper Scissors = Loss
play Scissors Rock = Loss
play Scissors Paper = Win
play _ _ = Draw

scoreHand :: Hand -> Int
scoreHand Rock = 1
scoreHand Paper = 2
scoreHand Scissors = 3

scoreOutcome :: Outcome -> Int
scoreOutcome Win = 6
scoreOutcome Loss = 0
scoreOutcome Draw = 3

missingHand :: Hand -> Outcome -> Hand
missingHand Rock Win = Scissors
missingHand Rock Loss = Paper
missingHand Paper Win = Rock
missingHand Paper Loss = Scissors
missingHand Scissors Win = Paper
missingHand Scissors Loss = Rock
missingHand x Draw = x

scorePlay :: (Hand, Hand) -> (Int, Int)
scorePlay (a, b) = (scoreA, scoreB)
  where
    scoreA = scoreHand a + scoreOutcome (play a b)
    scoreB = scoreHand b + scoreOutcome (play b a)

parseHands :: String -> (Hand, Hand)
parseHands line =
  (handA, handB)
  where
    handA = hand (head line)
    handB = hand (last line)

parseHandOutcome :: String -> (Hand, Outcome)
parseHandOutcome line =
  (handA, out)
  where
    handA = hand (head line)
    out = outcome (last line)

aoc2part1 :: String -> (Int, Int)
aoc2part1 file = playSum
  where
    plays = filter (not . null) (lines file)
    playHands = map parseHands plays
    playScores = map scorePlay playHands

    sum (accA, accB) (a, b) = (accA + a, accB + b)
    playSum = foldl sum (0, 0) playScores

aoc2part2 :: String -> (Int, Int)
aoc2part2 file = playSum
  where
    plays = filter (not . null) (lines file)
    playHandOutcomes = map parseHandOutcome plays

    missing (h, o) = (h, missingHand h o)
    playHands = map missing playHandOutcomes
    playScores = map scorePlay playHands

    sum (accA, accB) (a, b) = (accA + a, accB + b)
    playSum = foldl sum (0, 0) playScores

main :: IO ()
main = do
  contents <- readFile "day2.txt"
  print $ aoc2part1 contents

main2 :: IO ()
main2 = do
  contents <- readFile "day2.txt"
  print $ aoc2part2 contents
