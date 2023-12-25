
import System.IO
import Control.Monad
import Data.Char (isDigit)
import Debug.Trace
import Data.List (nub)

splitString :: String -> ([(Int, Int, Int)], [(Int, Int)])

splitString input = go (0, 0) Nothing ([], []) input
  where
    go (x, y) (Just (start_x, start_y, n)) acc (c:cs)
      | isDigit c = go (x + 1, y) (Just (start_x, start_y, n * 10 + fromEnum c - fromEnum '0')) acc cs
      | c == '.'  = go (x + 1, y) Nothing ((start_x, start_y, n) : fst acc, snd acc) cs
      | c == '\n' = go (0, y + 1) Nothing ((start_x, start_y, n) : fst acc, snd acc) cs
      | otherwise = go (x + 1, y) Nothing ((start_x, start_y, n) : fst acc, (x, y) : snd acc) cs
    go (x, y) Nothing acc (c:cs)
      | isDigit c = go (x + 1, y) (Just (x, y, fromEnum c - fromEnum '0')) acc cs
      | c == '.'  = go (x + 1, y) Nothing acc cs
      | c == '\n' = go (0, y + 1) Nothing acc cs
      | otherwise = go (x + 1, y) Nothing (fst acc, (x, y) : snd acc) cs
    go (x, y) (Just (start_x, start_y, n)) acc [] = (reverse ((start_x, start_y, n) : fst acc), reverse (snd acc))
    go (x, y) Nothing acc [] = (reverse $ fst acc, reverse $ snd acc)


splitStringStr :: String -> ([(Int, Int, String)], [(Int, Int)])

splitStringStr input = go (0, 0) Nothing ([], []) input
  where
    go (x, y) (Just (start_x, start_y, n)) acc (c:cs)
      | isDigit c = go (x + 1, y) (Just (start_x, start_y, n ++ [c])) acc cs
      | c == '.'  = go (x + 1, y) Nothing ((start_x, start_y, n) : fst acc, snd acc) cs
      | c == '\n' = go (0, y + 1) Nothing ((start_x, start_y, n) : fst acc, snd acc) cs
      | otherwise = go (x + 1, y) Nothing ((start_x, start_y, n) : fst acc, (x, y) : snd acc) cs
    go (x, y) Nothing acc (c:cs)
      | isDigit c = go (x + 1, y) (Just (x, y, [c])) acc cs
      | c == '.'  = go (x + 1, y) Nothing acc cs
      | c == '\n' = go (0, y + 1) Nothing acc cs
      | otherwise = go (x + 1, y) Nothing (fst acc, (x, y) : snd acc) cs
    go (x, y) (Just (start_x, start_y, n)) acc [] = (reverse ((start_x, start_y, n) : fst acc), reverse (snd acc))
    go (x, y) Nothing acc [] = (reverse $ fst acc, reverse $ snd acc)


find_adjacent :: (Int, Int, Int) -> [(Int, Int)] -> Int

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y
trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

-- find_adjacent num [] = 0
find_adjacent num symbols =
    let num_len = length $ show $ trd3 num
    in
        if length [x | x <- [(fst3 num) - 1..(fst3 num) + num_len], y <- [(snd3 num) - 1..(snd3 num) + 1], (x, y) `elem` symbols] > 0
        then trd3 num
        else 0

part_sum :: String -> Int

part_sum input =
    let
        nums_symbols = splitString input
    in
        foldl (\acc num -> acc + (find_adjacent num $ snd nums_symbols)) 0 (fst nums_symbols)

get_gear_ratio :: (Int, Int) -> [(Int, Int, String)] -> Int

get_gear_ratio symbol nums =
    let
        adjacents = [(x_n, y_n, num) | x <- [(fst symbol) - 1..(fst symbol) + 1], y <- [(snd symbol) - 1..(snd symbol) + 1], (x_n, y_n, num) <- nums,
                    x_n <= x && (x_n + length num - 1) >= x, y_n == y]
        u_adj = map (\(x, y, num) -> num) $ nub adjacents
    in
        if (length u_adj) == 2
        then
            (read (u_adj !! 0)::Int) * (read (u_adj !! 1)::Int)
        else 0

gear_ratios :: String -> Int

gear_ratios input =
    let
        nums_symbols = splitStringStr input
    in
        foldl (\acc symbol -> acc + (get_gear_ratio symbol $ fst nums_symbols)) 0 (snd nums_symbols)


main = do
    let test_input = "467..114..\n\
                    \...*......\n\
                    \..35..633.\n\
                    \......#...\n\
                    \617*......\n\
                    \.....+.58.\n\
                    \..592.....\n\
                    \......755.\n\
                    \...$.*....\n\
                    \.664.598..\n"
    contents <- readFile "input_day_3.txt"
    --print $ splitStringStr test_input
    print $ gear_ratios contents
    


