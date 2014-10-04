
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
    
-- Tests

testEmptyIsNotValid
    = isValid [] == False

testSingleItemListIsNotValid
    = isValid [red1] == False

testTwoItemsListIsNotValid
    = isValid [red1,red2] == False
    && isValid [red1,red1] == False
    && isValid [red5,red12] == False

testAllSameIsValid
    = isValid [red1,blue1,black1]
    && isValid [yellow3,blue3,red3]

testUnrelatedItemsListIsInvalid
    = isValid [red1,red3,red5] == False
    && isValid [red1,red1,red5] == False
    && isValid [red1,red5,red5] == False
    && isValid [red1,red1,red1,red5,red5] == False
    && isValid [red1,red1,red1,red5,red5,red5] == False
    
testOrderedListIsValid
    = isValid [red1,red2,red3]
    && isValid [red5,red6,red7,red8,red9,red10]
    && isValid [red1,red2,red3,red4,red5,red6,red7,red8,red9,red10,red11,red12,red13]

testPartlySortedListIsInvalid
    = isValid [red1,red2,red8] == False
    && isValid [red5,red10,red11] == False
    
testSeriesOfNonUniformColorsIsInvalid
    = isValid [red1, blue2, yellow3] == False
    && isValid [red5, red6, yellow7] == False
    && isValid [blue5, black10, black11] == False

testSequenceOfSameCannotHaveTheSameColorMoreThanOnce
    = isValid [red3, yellow3, red3] == False
    && isValid [red3, red3, blue3] == False
    && isValid [blue3, blue3, red3] == False
    
testJokerInSequenceOfSameIsValid
    = isValid [red3, blue3, joker]
    && isValid [joker, red3, blue3]
    && isValid [black3, joker, red3, blue3]
    
testJokerDoesBreakSequence
    = isValid [red4, joker, blue5] == False
    && isValid [red4, black4, joker, blue5] == False
    && isValid [red4, joker, black4, blue5] == False

testJokerInSeriesIsValid
    = isValid [red3, joker, red5]
    && isValid [red3, red4, joker]
    && isValid [joker, red3, red4]
    
testJokerDoesBreakSeries
    = isValid [red3, joker, red4] == False
    && isValid [red3, joker, blue5] == False
    
isValidSuite = testEmptyIsNotValid
    && testSingleItemListIsNotValid
    && testAllSameIsValid
    && testTwoItemsListIsNotValid
    && testUnrelatedItemsListIsInvalid
    && testOrderedListIsValid
    && testPartlySortedListIsInvalid
    && testSeriesOfNonUniformColorsIsInvalid
    && testSequenceOfSameCannotHaveTheSameColorMoreThanOnce
    && testJokerInSequenceOfSameIsValid
    && testJokerDoesBreakSequence
    && testJokerInSeriesIsValid
    && testJokerDoesBreakSeries
    

testCanInsertSameNumberTileToSequenceOfSame
    = canInsert [red4, blue4, black4] yellow4
    && canInsert [blue1, red1] yellow1

testCanInsertConsecutiveTileToSeries
    = canInsert [red1,red2,red3] red4
    && canInsert [red5,red6,red7] red4
    && canInsert [blue5,blue6] blue7
    && canInsert [blue5,blue6] blue4

canInsertSuite
    = testCanInsertSameNumberTileToSequenceOfSame
    && testCanInsertConsecutiveTileToSeries

testArrangeCanFormSequenceOfSame
    = let r = arrange [red1,yellow1,blue1]
    in snd r
    
arrangeSuite
    = testArrangeCanFormSequenceOfSame

allSuites = isValidSuite && canInsertSuite && arrangeSuite

main = if allSuites
    then print "ok"
    else print "Failure!!"


