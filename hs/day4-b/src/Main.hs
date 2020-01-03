module Main where

import Data.List (group)
import Data.List.Ordered (isSorted)
import Data.List.Split (endBy)

hasTwiceRepeatingDigit :: Int -> Bool
hasTwiceRepeatingDigit x = elem 2 $ map length $ group $ show x

isIncreasingLeftToRight :: Int -> Bool
isIncreasingLeftToRight x = isSorted (show x)

possiblePasswords :: Int -> Int -> [Int]
possiblePasswords min max = [p | p <- [min..max],
                                 isIncreasingLeftToRight p,
                                 hasTwiceRepeatingDigit p]

listToTuple2 :: [a] -> (a, a)
listToTuple2 (x:y:_) = (x, y)

addNewline :: String -> String
addNewline x = x ++ "\n"

processInput :: String -> String
processInput x = addNewline $ show $ length $ uncurry possiblePasswords $ listToTuple2 $ map read $ endBy "-" x

main :: IO ()
main = interact processInput
