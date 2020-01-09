import qualified Data.Vector as V (fromList, head, length)
import Data.Vector (Vector, (!), (//))
import Data.Maybe (listToMaybe)

type ProgramInputs = [Int]
type ProgramMemory = Vector Int
type ProgramCounter = Int

data Computer = Running ProgramInputs ProgramMemory ProgramCounter | Stopped ProgramMemory

initializeMemory :: [Int] -> ProgramMemory
initializeMemory = V.fromList

data Instruction = Add | Multiply | Scan | Print | Halt

fromInt :: Int -> Instruction
fromInt 1 = Add
fromInt 2 = Multiply
fromInt 3 = Scan
fromInt 4 = Print
fromInt 99 = Halt

currentInstruction :: Computer -> Instruction
currentInstruction (Running _ m pc) = fromInt (m ! pc)

indirectBinaryOperation :: (Int -> Int -> Int) -> Computer -> Computer
indirectBinaryOperation f (Running i m pc) =
    Running i (m // delta) (pc + 4)
  where
    ra = m ! (pc + 1)
    rb = m ! (pc + 2)
    rc = m ! (pc + 3)
    delta = [(rc, f (m ! ra) (m ! rb))]

add :: Computer -> Computer
add = indirectBinaryOperation (+)

multiply :: Computer -> Computer
multiply = indirectBinaryOperation (*)

scan :: Computer -> Computer
scan (Running (i:is) m pc) =
    Running is (m // delta) (pc + 2)
  where
    ra = m ! (pc + 1)
    delta = [(ra, i)]

print :: Computer -> IO Computer
print (Running i m pc) = do
    ra = m ! (pc + 1)
    print ra
    return (Running i m (pc + 2))

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

runProgram :: [String] -> ProgramMemory -> ProgramMemory
runProgram m = runNextInstruction (Running m 0)

addNewline :: String -> String
addNewline x = x ++ "\n"

processInput :: String -> String
processInput x =
    runProgram inputs $ initializeMemory $ map read $ code
  where
    allInput = lines x
    code = endBy "," $ head allInput
    inputs = tail allInput

main :: IO ()
main = interact processInput 
