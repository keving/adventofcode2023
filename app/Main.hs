{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad (void)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, tails, singleton, group, sort, sortBy, sortOn)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord
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

-- Day 4

data Card = Card {cardid::Int, winners::[Int], draws::[Int]} deriving Show

parseCard :: Parsec String () Card
parseCard = do
  pid <- string "Card" <* spaces *> many1 digit <* char ':' <* spaces <&> read
  pwinners <- many1 (many1 digit <* spaces <&> read)
  void $ char '|' >> spaces
  pdraws <- many1 (many1 digit <* spaces <&> read)
  return Card {cardid=pid, winners=pwinners, draws=pdraws}

day4 :: IO ()
day4 = do
  inp <- getContents
  let cardwins = map ((\(Card i ws ds) -> (i, length $ filter (`elem` ws) ds)) . fromRight (Card 0 [] []) . parse parseCard "(input)") $ lines inp
  print $ foldr (\(_, w) a -> a + if w == 0 then 0 else 2^(w-1)) (0::Int) cardwins
  print $ length $ loop (\xs -> concat [take w $ drop i cardwins | (i, w) <- xs]) cardwins
  where
    loop :: ([a] -> [a]) -> [a] -> [a]
    loop _ [] = []
    loop f xs = xs ++ loop f (f xs)

parseSeedMaps :: Parsec String () ([[Int]], Map.Map String (String, Int -> Int))
parseSeedMaps = do
  seeds <- string "seeds:" <* spaces *> many1 (count 2 $ many1 digit <* spaces <&> read)
  maps <- many $ do
    fromw <- many1 letter <* string "-to-"
    tow <- many1 letter <* spaces <* string "map:" <* spaces
    map_ranges <- many $ do
      dest_start <- many1 digit <* spaces <&> read
      src_start <- many1 digit <* spaces <&> read
      len <- many1 digit <* spaces <&> read
      return (dest_start, src_start, len)
    return (fromw, (tow, mk_map map_ranges))
  return (seeds, Map.fromList maps)
  where
    mk_map :: [(Int, Int, Int)] -> Int -> Int
    mk_map [] x = x
    mk_map ((d,s,l):ys) x | x >= s && x < s+l = d + x - s
                          | otherwise = mk_map ys x

day5:: IO ()
day5 = do
  inp <- getContents
  let (seeds, maps) = fromRight ([], Map.empty) $ parse parseSeedMaps "(input)" inp
  print $ minimum [get_location maps "seed" x | [s,l] <- seeds, x <- [s..s+l]]
  print "Done"
  where
    get_location :: Map.Map String (String, Int -> Int) -> String -> Int -> Int
    get_location _ "location" n = n
    get_location m t n = get_location m d (f n)
      where (d,f) = m ! t

-- Day 6

parseRaceRounds :: Parsec String () (Int, Int)
parseRaceRounds = do
  times <- string "Time:" <* spaces *> many1 (many1 digit <* spaces)
  distances <- string "Distance:" <* spaces *> many1 (many1 digit <* spaces)
  return (read $ concat times, read $ concat distances)

day6 :: IO ()
day6 = do
  inp <- getContents
  let (t, d) = fromRight (0,0) $ parse parseRaceRounds "(input)" inp
  print $ length $ calc_winning_distances t d
  where
    calc_winning_distances :: Int -> Int -> [Int]
    calc_winning_distances t d = [(t-at)*at | at <- [0..t], (t-at)*at > d]

-- Day 7

data CamelCard = Ace | King | Queen | Jack | Ten | NonPicture Int deriving (Eq, Show)
data CamelHand = High | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Show)

cardRank :: CamelCard -> Int
cardRank Ace = 14
cardRank King = 13
cardRank Queen = 12
-- Jacks are Jokers, but worth least.
cardRank Jack = 1
cardRank Ten = 10
cardRank (NonPicture v) = v

instance Ord CamelCard where x <= y = cardRank x <= cardRank y

instance Read CamelCard
  where
    readsPrec _ ('A':xs) = [(Ace, xs)]
    readsPrec _ ('K':xs) = [(King, xs)]
    readsPrec _ ('Q':xs) = [(Queen, xs)]
    readsPrec _ ('J':xs) = [(Jack, xs)]
    readsPrec _ ('T':xs) = [(Ten, xs)]
    readsPrec _ (x:xs) = [(NonPicture $ read [x], xs)]
    readsPrec _ [] = []

camelType :: [CamelCard] -> CamelHand
camelType = getType . sortBy (comparing Data.Ord.Down) . map length . group . sort
  where
    getType :: [Int] -> CamelHand
    getType (5:_) = FiveOfAKind
    getType (4:_) = FourOfAKind
    getType (3:2:_) = FullHouse
    getType (3:_) = ThreeOfAKind
    getType (2:2:_) = TwoPair
    getType (2:_) = OnePair
    getType _ = High

parseHands :: Parsec String () [([CamelCard], Int)]
parseHands = many $ do
  hand <- count 5 alphaNum <* spaces <&> map (read . singleton)
  bid <- many1 digit <* spaces <&> read
  return (hand, bid)

day7 :: IO ()
day7 = do
  inp <- getContents
  let hands = sortBy orderHands $ map addJokerHands $ fromRight [] $ parse parseHands "(input)" inp
  print hands
  print $ sum $  zipWith (*) [1..length hands] (map (\(_,_,x) -> x) hands)
  where
    -- Convert Jokers to the card with most representatives.
    addJokerHands :: ([CamelCard], Int) -> ([CamelCard], [CamelCard], Int)
    addJokerHands (h,b) = (h, map (\c -> if c == Jack then top_card else c) h, b)
      where
        top_card :: CamelCard
        top_card = if null c then Ace else head $ last c
          where
            c :: [[CamelCard]]
            c = sortOn length . group $ sort $ filter (/= Jack) h
    orderHands :: ([CamelCard], [CamelCard], Int) -> ([CamelCard], [CamelCard], Int) -> Ordering
    orderHands (xs,jxs,_) (ys,jys,_) | camelType jxs == camelType jys = compare xs ys
                             | otherwise = compare (camelType jxs) (camelType jys)

-- Day 8

parseRoute :: Parsec String () ([Char], Map.Map String (Map.Map Char String))
parseRoute = do
  route <- spaces *> many1 letter <* spaces
  graph <- many1 $ do
    node <- many1 alphaNum <* spaces <* char '=' <* spaces
    dirs <- char '(' *>  spaces *> sepBy (many1 alphaNum <* spaces) (char ',' >> spaces) <* char ')' <* spaces
    return (node, Map.fromList (zip "LR" dirs))
  return (cycle route, Map.fromList graph)


day8 :: IO ()
day8 = do
  inp <- getContents
  let (path, graph) = fromRight ([], Map.empty) $ parse parseRoute "(input)" inp
  print $ subtract 1 . length $ travel graph ["AAA"] path
  where
    travel :: Map.Map String (Map.Map Char String) -> [String] -> String -> [String]
    travel _ rs@("ZZZ":_) _ = rs
    travel g rs@(r:_) (p:ps) = travel g ((g!r)!p:rs) ps

main :: IO ()
main = day8
