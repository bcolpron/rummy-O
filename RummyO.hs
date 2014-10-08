
module RummyO where

import Tiles

colorCount :: [Tile] -> Color -> Int
colorCount [] _ = 0
colorCount (Joker:xs) _ = 0
colorCount (x:xs) c =
    if color x == c
    then colorCount xs c + 1
    else colorCount xs c

isSequenceOfSame :: [Tile] -> Bool
isSequenceOfSame (x:[]) = True
isSequenceOfSame (Joker:xs) = isSequenceOfSame xs
isSequenceOfSame (x:Joker:xs) = isSequenceOfSame (x:xs)
isSequenceOfSame (x:xs)
    = number x == number (head xs)
    && isSequenceOfSame xs
    && colorCount xs (color x) == 0


isSeries :: [Tile] -> Bool
isSeries (x:[]) = True
isSeries (Joker:xs) = isSeries xs
isSeries (x:Joker:[]) = True
isSeries (x:Joker:xs)
    = number x + 2 == number (head xs)
    && color x == color (head xs)
    && isSeries xs
isSeries (x:xs)
    = number x + 1 == number (head xs)
    && color x == color (head xs)
    && isSeries xs

isValid :: [Tile] -> Bool
isValid xs = length xs >= 3 && (isSequenceOfSame xs || isSeries xs)

canInsert :: [Tile] -> Tile -> Bool
canInsert xs x = isValid (xs ++ [x]) || isValid (x:xs)

arrange :: [Tile] -> ([Tile], Bool)
arrange x
    = (x, isValid x)

comb :: [a] -> Int -> [[a]]    
comb l n
    | n == 1 = map (:[]) l
    | length l == n = [l]
    | otherwise = (map ((head l):) (comb (tail l) (n-1))) ++ (comb (tail l) n)
    