module Main where

import Data.List (group)
import Data.List.Ordered (isSorted)
import Data.List.Split (endBy)

hasRepeatingDigit :: Int -> Bool
hasRepeatingDigit x =
  let ps = show x
  in (length $ group ps) < (length ps)

isIncreasingLeftToRight :: Int -> Bool
isIncreasingLeftToRight x = isSorted (show x)

possiblePasswords :: Int -> Int -> [Int]
possiblePasswords min max = [p | p <- [min..max],
                                 isIncreasingLeftToRight p,
                                 hasRepeatingDigit p]

listToTuple2 :: [a] -> (a, a)
listToTuple2 (x:y:_) = (x, y)

addNewline :: String -> String
addNewline x = x ++ "\n"

processInput :: String -> String
processInput x = addNewline $ show $ length $ uncurry possiblePasswords $ listToTuple2 $ map read $ endBy "-" x

main :: IO ()
main = interact processInput
