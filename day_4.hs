

import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
--import qualified Data.Set as Set
import Debug.Trace


import qualified Data.HashTable.IO as H
import qualified Control.Monad.Trans.Accum as H
type HashTable k v = H.BasicHashTable k v


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


type Card = (Int, ([Int], [Int]))
get_cards_ht :: String -> IO (HashTable Int Card)

get_cards_ht input = do
        ht <- H.new
        let cards = lines input
        let cards' = map (\card -> last $ splitOn ":" card) cards
        let cards'' = map (\card -> (map read $ words $ head $ splitOn "|" card,
                                 map read $ words $ last $ splitOn "|" card)) cards'
        mapM_ (\(i, card) -> H.insert ht i (1, card)) (zip [1..] cards'')
        return ht

matches :: Card -> Int
matches card =
    sum [1 | x <- (fst $ snd card), y <- (snd $ snd card), x == y]

getRange :: Int -> Int -> [a] -> [a]
getRange start end = take (end - start + 1)

total_cards :: HashTable Int Card -> IO Int
total_cards ht = do
    cards_list <- H.toList ht
    let updateCounts = mapM_ (updateCard ht) $ reverse cards_list
    updateCounts
    final_cards_list <- H.toList ht
    print $ reverse final_cards_list
    return $ sum $ map (fst . snd) final_cards_list

-- Helper function to update the counts of subsequent cards
updateCard :: HashTable Int Card -> (Int, Card) -> IO ()
updateCard ht (i, _) = do
    cardM <- H.lookup ht i
    case cardM of
        Nothing -> return ()
        Just card -> do
            let copies = fst card
            let matchCount = matches card
            -- Update subsequent cards based on the number of matches
            if matchCount == 0 then return ()
            else do
                print $ "Updating at card " ++ show i ++ " with " ++ show copies ++ " copies"
                mapM_ (\j -> H.mutate ht j (\(Just (count, card2)) ->
                            (Just (count + copies, card2), ()))) [i+1 .. i+matchCount]

main = do
    let test_input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
                     \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
                     \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
                     \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
                     \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
                     \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    content <- readFile "input_day_4.txt"
    ht <- get_cards_ht content
    total <- total_cards ht
    print total
    --print $ points_sum content