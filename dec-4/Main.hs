{-# LANGUAGE GHC2021    #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.List.Split (splitOn)
import qualified Data.Set        as S

type Set a = S.Set a

input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

trim = reverse . trimFront . reverse . trimFront
  where
    trimFront = \case
      (' ':xs) -> trimFront xs
      xs -> xs

parseLine :: String -> [Set Int]
parseLine
  = map ( S.fromList
        . map (read :: String -> Int)
        . filter (not . null)
        . splitOn " "
        )
  . splitOn "|"
  . (!! 1)
  . splitOn ":"

parseInput :: String -> [Int]
parseInput = map (S.size . foldl1 S.intersection . parseLine) . lines 

part1 :: [Int] -> Int
part1 input =
  let m1 x = x - 1
      calc = \case
        0 -> 0
        n | n > 1 -> 2^(n-1)
        n -> 1
  in sum . map calc $ input

part2 :: [Int] -> Int
part2 cards =
  let addLists = \cases
        [] ys               -> []
        xs []               -> xs
        ((x, x'):xs) (y:ys) -> (x, x' + y) : addLists xs ys
      combine = \case
        []                 -> []
        ((wins, cards):xs) -> cards : combine (addLists xs (replicate wins cards))
  in sum . combine . zip cards $ replicate (length cards) 1

main = interact $ (++"\n") . show . (\x -> (part1 x, part2 x)) . parseInput
