{-# LANGUAGE GHC2021 #-}

module Main where

import Debug.Trace (traceShowId)
import Data.List (nub)
import qualified Data.Set as S
import qualified Data.Map as M
type Set a = S.Set a
type Map k v = M.Map k v

input :: String
input = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

symbols :: String
symbols = "/*@%$+=-#&"

first f (a, b) = (f a, b)
second f (a, b) = (a, f b)

enumerate2D = zipWith (\y v -> zipWith (\x v' -> ((x, y), v'))
                                        [0..]
                                        v)
                      [0..]

isDigit :: Char -> Bool
isDigit c = c `elem` ("0123456789" :: String)

part1 input =
  let
      width = length . head . lines $ input
      height = length . lines $ input
      values = M.fromList . concat . enumerate2D . lines $ input
      symbolPositions = M.keys . M.filter (`elem` symbols) $ values
      surround (x,y) = filter (\(x, y) -> x >= 0 && x < width && y >= 0 && y < height)
                              [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]
      expand (x,y) = filter (\(x, y) -> x >= 0 && x < width) [(x + dx, y) | dx <- [-1..1]]
      filterDots pos = isDigit $ values M.! pos
      expandCandidates cs = let cs' = filter filterDots . nub . concatMap expand $ cs
                            in if cs == cs'
                                then cs
                                else expandCandidates cs'
      firstGen = concatMap (filter filterDots . surround) symbolPositions
      filterNeighbours ns = let set = S.fromList ns
                                m1 x = x - 1
                            in filter (\p -> not $ first m1 p `S.member` set) ns
      numberPositions = filterNeighbours . expandCandidates $ firstGen
      toFlat (x, y) = x + (width + 1) * y
  in sum
    . map ( read
          . takeWhile isDigit
          . (`drop` input)
          . toFlat
          )
    $ numberPositions

part2 input =
  let
      width = length . head . lines $ input
      height = length . lines $ input
      values = M.fromList . concat . enumerate2D . lines $ input
      symbolPositions = M.keys . M.filter (== '*') $ values
      surround (x,y) = filter (\(x, y) -> x >= 0 && x < width && y >= 0 && y < height)
                              [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]
      expand (x,y) = filter (\(x, y) -> x >= 0 && x < width) [(x + dx, y) | dx <- [-1..1]]
      filterDots pos = isDigit $ values M.! pos
      expandCandidates cs = let cs' = filter filterDots . nub . concatMap expand $ cs
                            in if cs == cs'
                                then cs
                                else expandCandidates cs'
      firstGen = map (filter filterDots . surround) symbolPositions
      filterNeighbours ns = let set = S.fromList ns
                                m1 x = x - 1
                            in filter (\p -> not $ first m1 p `S.member` set) ns
      numberPositions = map (filterNeighbours . expandCandidates) firstGen
      toFlat (x, y) = x + (width + 1) * y
  in sum
    . map ( product
          . map ( read
                . takeWhile isDigit
                . (`drop` input)
                )
          )
    . filter ((== 2) . length)
    . map (map toFlat)
    $ numberPositions

main = interact $ (++"\n") . show . (\x -> (part1 x, part2 x))
