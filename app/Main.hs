{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.Map as Map
import Data.Either (fromRight)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)
import Text.Parsec
import Text.Regex.TDFA

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

-- Day 3

day3 :: IO ()
day3 = do
  -- Part 1
  inp <- getContents
  let ls = lines inp
  let els = zip [0..] $ [replicate (length (head ls) + 2) '.'] ++
                        map (\l -> ['.'] ++ l ++ ['.']) ls ++
                        [replicate (length (head ls) + 2) '.']
  let matches = map (\(n, s) -> (n, getAllMatches (s =~ "([0-9]+)") :: [(Int, Int)])) els
  let augmatches = concatMap (\(n, ms) -> [(n, read (take l $ drop s (snd $ els!!n))::Int, m) | m@(s,l) <- ms]) matches
  let tochecks = map (\(n, v, (s,l)) -> (v, [(n-1,p) | p <- [(s-1)..(s+l)]] ++
                                          [(n, s-1), (n, s+l)] ++
                                          [(n+1,p) | p <- [(s-1)..(s+l)]]))
                     augmatches
  print $ sum $ map fst $ filter (\(_,checks) -> any (\(r,c) -> snd (els!!r)!!c `notElem` '.':['0'..'9']) checks) tochecks
  -- Part 2
  let starmatches = concatMap (\(n, s) -> map ((n,) . fst) (getAllMatches (s =~ "\\*") :: [(Int, Int)])) els
  let gearns = map (\(r,c) -> [d | (n, d, (s,l)) <- augmatches, (n,s,l) `overlaps` (r,c)]) starmatches
  print $ foldr (\xs acc -> acc + mulpairs xs) 0 gearns
  where overlaps (row,start,len) (starrow,starcol) =
          starrow `elem` [(row-1)..(row+1)] && starcol `elem` [(start-1)..(start+len)]
        mulpairs [x, y] = x*y
        mulpairs _ = 0

main :: IO ()
main = day3
