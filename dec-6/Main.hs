{-# LANGUAGE GHC2021             #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import           Data.List       (nub)
import           Data.List.Split (splitOn)
import qualified Data.Map        as M
import qualified Data.Maybe      as O
import qualified Data.Set        as S
import           Debug.Trace     (traceShowId)

type Set a = S.Set a

type Map k v = M.Map k v

input = "Time:      7  15   30\nDistance:  9  40  200"

first f (a, b) = (f a, b)
second f (a, b) = (a, f b)

part1 input =
  let [time, distance] = map (map (read :: String -> Int) . filter (not . null) . splitOn " " . drop 11) . lines $ input
      candidates t = [0..t-1]
      compete :: Int -> Int -> Int
      compete t d = length . filter (> d) . map (\x -> (t - x) * x) $ [0..t-1]
 in product . zipWith compete time $ distance
  
part2 input =
  let [time, distance] = map ((read :: String -> Int) . filter (/= ' ') . drop 11) . lines $ input
      candidates t = [0..t-1]
      compete :: Int -> Int -> Int
      compete t d = length . filter (> d) . map (\x -> (t - x) * x) $ [0..t-1]
 in compete time distance

main = interact $ (++ "\n") . show . (\x -> (part1 x, part2 x))
