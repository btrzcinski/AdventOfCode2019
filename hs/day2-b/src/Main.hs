import qualified Data.Vector as V (fromList, head, length)
import Data.Vector (Vector, (!), (//))
import Data.Maybe (listToMaybe)

type ProgramMemory = Vector Int
type ProgramCounter = Int

data Computer = Running ProgramMemory ProgramCounter | Stopped ProgramMemory

initializeMemory :: [Int] -> ProgramMemory
initializeMemory = V.fromList

data Instruction = Add | Multiply | Halt

fromInt :: Int -> Instruction
fromInt 1 = Add
fromInt 2 = Multiply
fromInt 99 = Halt

currentInstruction :: Computer -> Instruction
currentInstruction (Running m pc) = fromInt (m ! pc)

indirectBinaryOperation :: (Int -> Int -> Int) -> Computer -> Computer
indirectBinaryOperation f (Running m pc) =
    Running (m // delta) (pc + 4)
  where
    ra = m ! (pc + 1)
    rb = m ! (pc + 2)
    rc = m ! (pc + 3)
    delta = [(rc, f (m ! ra) (m ! rb))]

add :: Computer -> Computer
add = indirectBinaryOperation (+)

multiply :: Computer -> Computer
multiply = indirectBinaryOperation (*)

halt :: Computer -> Computer
halt (Running m _) = Stopped m

runNextInstruction :: Computer -> ProgramMemory
runNextInstruction (Stopped m) = m
runNextInstruction c@(Running _ _) =
    runNextInstruction nextState
  where
    nextState = case (currentInstruction c) of
                  Add -> add c
                  Multiply -> multiply c
                  Halt -> halt c

runProgram :: ProgramMemory -> ProgramMemory
runProgram m = runNextInstruction (Running m 0)

getProgramOutput :: ProgramMemory -> Int
getProgramOutput m = V.head $ runProgram m

getProgramOutputForState :: (Int, Int) -> ProgramMemory -> Int
getProgramOutputForState (noun, verb) m = getProgramOutput (m // [(1, noun), (2, verb)])

searchForNounVerb :: Int -> ProgramMemory -> Maybe (Int, Int)
searchForNounVerb output m =
    listToMaybe $ filter (\x -> (getProgramOutputForState x m) == output) possibilities
  where
    possibilities = [(noun, verb) | noun <- [1..100], verb <- [1..100]]

addNewline :: String -> String
addNewline x = x ++ "\n"

processInput :: String -> String
processInput x =
  addNewline $ show $ searchForNounVerb 19690720 $ initializeMemory $ map read $ lines x

main :: IO ()
main = interact processInput 
