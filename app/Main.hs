module Main where

import qualified Data.Map as Map
import Data.Either (fromRight)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)
import Text.Parsec

-- Day 1

numbers :: [(String, Int)]
numbers = [(show n, n) | n <- [0..9]] ++
  [("zero", 0),
   ("one", 1),
   ("two", 2),
   ("three", 3),
   ("four", 4),
   ("five", 5),
   ("six", 6),
   ("seven", 7),
   ("eight", 8),
   ("nine", 9)]

toCV :: String -> Int
toCV w
  | null digits = 0
  | otherwise   = head digits * 10 + last digits
  where
    -- Find digits for every substring in w.
    digits = concatMap (\sub -> mapMaybe (\(p,d) -> if p `isPrefixOf` sub then Just d else Nothing) numbers) $ tails w

day1 :: IO ()
day1 = do
       inp <- getContents
       let cv = sum $ map toCV $ words inp
       print cv

-- Day 2

data Colors = Red | Green | Blue

data Sample = Sample {reds::Int, greens::Int, blues::Int} deriving Show

data Game = Game {gameid::Int, samples::[Sample]} deriving Show

bag :: Sample
bag = Sample {reds=12, greens=13, blues=14}

parseGame :: Parsec String () Game
parseGame = do
  _ <-  string "Game"
  spaces
  gid <- many1 digit
  _ <- char ':'
  spaces
  gsamplesMapList <- sepBy (
    do
      gsample <- sepBy (
        do
          n <- many1 digit
          spaces
          c <- many1 letter
          return (c, read n))
        (spaces >> char ',' >> spaces)
      return $ Map.fromList gsample)
    (spaces >> char ';' >> spaces)
  return Game {gameid=read gid,
               samples=map (\m -> Sample {reds=Map.findWithDefault 0 "red" m,
                                          greens=Map.findWithDefault 0 "green" m,
                                          blues=Map.findWithDefault 0 "blue" m
                                         }) gsamplesMapList}

day2 :: IO ()
day2 = do
  inp <- getContents
  let games = map (fromRight (Game 0 []). parse parseGame "(input)") $ lines inp
  print $ sum [gameid g |
                g <- filter (all (\s -> reds s <= reds bag && greens s <= greens bag && blues s <= blues bag) . samples) games]
  print $ sum $ map ((\s -> reds s * greens s * blues s) . (foldr (\(Sample mr mg mb) (Sample r g b) -> Sample (mr `max` r) (mg `max` g) (mb `max` b)) (Sample 0 0 0) . samples)) games

main :: IO ()
main = day2
