

import Data.List
import Data.List.Split (splitOn)
--import qualified Data.Set as Set
import Debug.Trace


get_cards :: String -> [([Int], [Int])]

get_cards input =
    let
        cards = lines input
        cards' = map (\card -> last $ splitOn ":" card) cards
        cards'' = map (\card -> (map read $ words $ head $ splitOn "|" card,
                                 map read $ words $ last $ splitOn "|" card)) cards'
    in cards''


card_points :: ([Int], [Int]) -> Int

card_points card =
    let
        matches = [1 | x <- (fst card), y <- (snd card), x == y]
        points = if length matches == 0 then 0 else 2 ^ (length matches - 1)
    in
        points


points_sum :: String -> Int

points_sum input =
    let cards = get_cards input
    in foldl (\acc card -> acc + card_points card) 0 cards


main = do
    let test_input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
                     \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
                     \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
                     \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
                     \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
                     \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    content <- readFile "input_day_4.txt"
    --print $ get_cards test_input
    print $ points_sum content