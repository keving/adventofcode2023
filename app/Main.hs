module Main where

import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)

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

main :: IO ()
main = day1
