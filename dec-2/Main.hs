{-# LANGUAGE GHC2021    #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.List.Split hiding (endsWith, startsWith)

data Subset = Subset
  { red   :: Int
  , green :: Int
  , blue  :: Int
  } deriving (Show, Eq)

type Game = [Subset]

empty = Subset 0 0 0

dropEnd n = reverse . drop n . reverse

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith x ys = startsWith (reverse x) (reverse ys)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith =
  \cases
    _ []                   -> True
    (x:xs) (y:ys) | x == y -> startsWith xs ys
    _ _                    -> False

trim :: String -> String
trim =
  \case
    (' ':cs) -> trim cs
    cs       -> cs

parse :: String -> Game
parse
  = map (parseSubset empty . splitOn ",")
  . splitOn ";"
  . drop 2
  . dropWhile (/= ':')

parseSubset s =
  \case
    [] -> s
    (x:xs)
      | x `endsWith` " red"   ->
        let r = read . dropEnd 4 $ x
         in parseSubset s {red = r} xs
    (x:xs)
      | x `endsWith` " green" ->
        let g = read . dropEnd 5 $ x
         in parseSubset s {green = g} xs
    (x:xs)
      | x `endsWith` " blue"  ->
        let b = read . dropEnd 5 $ x
         in parseSubset s {blue = b} xs

part1 :: [Game] -> Int
part1
  = sum
  . map fst
  . filter (all possible . snd)
  . zip [1 ..]
  where
    possible (Subset r g b) = r <= 12 && g <= 13 && b <= 14

part2 :: [Game] -> Int
part2
  = sum
  . map ((\(Subset r g b) -> r * b * g)
         . foldl subsetMax empty
         )

subsetMax :: Subset -> Subset -> Subset
subsetMax (Subset r1 g1 b1) (Subset r2 g2 b2) =
  Subset (max r1 r2) (max g1 g2) (max b1 b2)

main = interact
  $ (++ "\n")
  . show
  . (\x -> (part1 x, part2 x))
  . map parse
  . lines
