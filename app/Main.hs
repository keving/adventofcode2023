module Main where

import Data.Char (isDigit)

toCV :: String -> Int
toCV w
  | null ld = 0
  | otherwise  = read $ head ld:[head rd]
  where
    ld = dropWhile (not . isDigit) w
    rd = dropWhile (not . isDigit) $ reverse w


day1 :: IO ()
day1 = do
       inp <- getContents
       let cv = sum $ map toCV $ words inp
       print cv

main :: IO ()
main = day1
