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

input =
  "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"

trim = reverse . trimFront . reverse . trimFront
  where
    trimFront =
      \case
        (' ':xs) -> trimFront xs
        xs -> xs

headOr =
  \cases
    _ (x:_) -> x
    x [] -> x

data Block = Block
  { dest :: Integer
  , src  :: Integer
  , len  :: Integer
  } deriving (Show, Eq)

data Range = Range
  { start :: Integer
  , end   :: Integer
  } deriving (Show, Eq)

part1 input =
  let parseB block =
        let (x:xs) = lines block
            [from, _, to] = splitOn "-" . head . splitOn " " $ x
            parseR [d, s, l] = Block d s l
            ranges = map (parseR . map read . splitOn " ") xs
         in ranges
      blockSrc = splitOn "\n\n" input
      blocks = map parseB . drop 1 $ blockSrc
      seeds = (map read . drop 1 . splitOn " " . head $ blockSrc) :: [Integer]
      transformLayer :: Integer -> Block -> Maybe Integer
      transformLayer seed b
        | b.src <= seed && seed < b.src + b.len = Just $ seed - b.src + b.dest
        | otherwise = Nothing
      transform :: [Block] -> Integer -> Integer
      transform block seed =
        headOr seed . O.mapMaybe (transformLayer seed) $ block
      transforms = foldl1 (.) (reverse . map transform $ blocks)
   in minimum . map transforms $ seeds

part2 input =

  let parseB block =
        let (x:xs) = lines block
            [from, _, to] = splitOn "-" . head . splitOn " " $ x
            parseR [d, s, l] = Block d s l
            ranges = map (parseR . map read . splitOn " ") xs
         in ranges
      blockSrc = splitOn "\n\n" input
      blocks = map parseB . drop 1 $ blockSrc
      seedSrc = (map read . drop 1 . splitOn " " . head $ blockSrc) :: [Integer]
      transformLayer :: Range -> Block -> [Range]
      transformLayer (Range start end) (Block d s l)
        | s <= start && end <= s + l = [Range (start - s + d) (end - s + d)]
        | s <= start && s + l <= end = [Range (start - s + d) (s+l), Range (d+l) end]
        | start <= s && end <= s + l = [Range d (d+l), Range (s+l) (end - s + d)]
        | start <= s && s + l <= end = []
        | otherwise                  = [Range start end]
      transform :: [Block] -> Range -> [Range]
      transform block = concatMap (\s -> map (transformLayer s) block)
      transforms :: Range -> [Range]
      transforms = foldl1 (.) (reverse . map transform $ blocks) . (:[])
      pairs =
        \case
          (x:y:xs) -> (x, y) : pairs xs
          [] -> []
      toRange (x, y) = Range x (x+y)
      seeds = nub . map toRange . pairs $ seedSrc
   in minimum . map start . traceShowId . concatMap transforms . traceShowId $ seeds

main = interact $ (++ "\n") . show . (\x -> (part1 x, part2 x))
