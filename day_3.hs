
import System.IO
import Control.Monad
import Data.Char (isDigit)

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

main = do
    -- let test_input = "467..114..\n\
    --                 \...*......\n\
    --                 \..35..633.\n\
    --                 \......#...\n\
    --                 \617*......\n\
    --                 \.....+.58.\n\
    --                 \..592.....\n\
    --                 \......755.\n\
    --                 \...$.*....\n\
    --                 \.664.598.."
    contents <- readFile "input_day_3.txt"
    print $ part_sum contents
    


