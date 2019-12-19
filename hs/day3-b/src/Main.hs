{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List.Split (endBy)
import Data.List (groupBy, intersectBy, sortOn)

toDelta :: String -> (Int, Int, Int)
toDelta (d:magStr) =
  let mag :: Int = read magStr
  in case d of
    'R' -> (mag, 0, mag)
    'L' -> (-mag, 0, mag)
    'U' -> (0, mag, mag)
    'D' -> (0, -mag, mag)

tupleSum :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
tupleSum (a, b, c) (d, e, f) = (a + d, b + e, c + f)

toVertices :: [(Int, Int, Int)] -> [(Int, Int, Int)]
toVertices deltas = scanl tupleSum (0, 0, 0) deltas

expand :: (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int, Int)]
expand (x0, y0, c0) (x1, y1, c1)
    | dx == 0 = [(x0, y, c) | (y, c) <- zip (if y0 < y1 then [y0..y1] else [y0,y0-1..y1]) [c0..c1]]
    | dy == 0 = [(x, y0, c) | (x, c) <- zip (if x0 < x1 then [x0..x1] else [x0,x0-1..x1]) [c0..c1]]
  where
    dx = x1 - x0
    dy = y1 - y0

toSpaces :: [(Int, Int, Int)] -> [(Int, Int, Int)]
toSpaces vertices = filter (\x -> x /= (0, 0, 0)) $ concat $ zipWith expand (init vertices) (tail vertices)

directionsToSpaces :: String -> [(Int, Int, Int)]
directionsToSpaces dirs = toSpaces $ toVertices $ map toDelta $ endBy "," dirs

manhattanDistance :: (Int, Int, Int) -> Int
manhattanDistance (x, y, _) = (abs x) + (abs y)

traversalCost :: (Int, Int, Int) -> Int
traversalCost (_, _, c) = c

coordinateEq :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
coordinateEq (x0, y0, _) (x1, y1, _) = (x0, y0) == (x1, y1)

coordinates :: (Int, Int, Int) -> (Int, Int)
coordinates (x, y, _) = (x, y)

closestIntersection :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> (Int, Int, Int)
closestIntersection a b = head $ sortOn traversalCost $ map (foldl1 (\(x0, y0, c0) (x1, y1, c1) -> (x0, y0, c0 + c1))) $ groupBy coordinateEq $ sortOn coordinates $ ((intersectBy coordinateEq a b) ++ (intersectBy coordinateEq b a))

listToTuple2 :: [a] -> (a, a)
listToTuple2 (x:y:_) = (x, y)

addNewline :: String -> String
addNewline s = s ++ "\n"

processInput :: String -> String
processInput x = addNewline $ show $ traversalCost $ uncurry closestIntersection $ listToTuple2 $ map directionsToSpaces $ lines x

main :: IO ()
main = interact processInput
