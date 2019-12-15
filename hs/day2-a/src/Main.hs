import qualified Data.Vector as V (fromList, head, length)
import Data.Vector (Vector, (!), (//))

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

restoreAlarmState :: ProgramMemory -> ProgramMemory
restoreAlarmState m = m // [(1, 12), (2, 2)]

addNewline :: String -> String
addNewline x = x ++ "\n"

processInput :: String -> String
processInput x =
  addNewline $ show $ V.head $ runProgram $ restoreAlarmState $ initializeMemory $ map read $ lines x

main :: IO ()
main = interact processInput 
