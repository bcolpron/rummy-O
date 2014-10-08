
import Test.HUnit
import Tiles
import RummyO

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

tests = TestCase (assertBool "old stuff" allSuites)

main = runTestTT tests
