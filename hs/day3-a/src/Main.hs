{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List.Split (endBy)
import Data.List (intersect, sortOn)

toDelta :: String -> (Int, Int)
toDelta (d:magStr) =
  let mag :: Int = read magStr
  in case d of
    'R' -> (mag, 0)
    'L' -> (-mag, 0)
    'U' -> (0, mag)
    'D' -> (0, -mag)

tupleSum :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleSum (a, b) (c, d) = (a + c, b + d)

toVertices :: [(Int, Int)] -> [(Int, Int)]
toVertices deltas = scanl tupleSum (0, 0) deltas

expand :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
expand (x0, y0) (x1, y1)
    | dx == 0 = [(x0, y) | y <- if y0 < y1 then [y0..y1] else [y1..y0]]
    | dy == 0 = [(x, y0) | x <- if x0 < x1 then [x0..x1] else [x1..x0]]
  where
    dx = x1 - x0
    dy = y1 - y0

toSpaces :: [(Int, Int)] -> [(Int, Int)]
toSpaces vertices = filter (\x -> x /= (0, 0)) $ concat $ zipWith expand (init vertices) (tail vertices)

directionsToSpaces :: String -> [(Int, Int)]
directionsToSpaces dirs = toSpaces $ toVertices $ map toDelta $ endBy "," dirs

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = (abs x) + (abs y)

closestIntersection :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int)
closestIntersection a b = head $ sortOn manhattanDistance $ intersect a b

listToTuple2 :: [a] -> (a, a)
listToTuple2 (x:y:_) = (x, y)

addNewline :: String -> String
addNewline s = s ++ "\n"

processInput :: String -> String
processInput x = addNewline $ show $ manhattanDistance $ uncurry closestIntersection $ listToTuple2 $ map directionsToSpaces $ lines x

main :: IO ()
main = interact processInput
