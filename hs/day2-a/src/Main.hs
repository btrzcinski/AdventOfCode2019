import qualified Data.Vector as V

initializeMemory :: [Int] -> V.Vector
initializeMemory = V.fromList

addNewline :: String -> String
addNewline x = x ++ "\n"

runProgram :: String -> String
runProgram x =
  addNewline $ show $ V.length $ initializeMemory $ map read $ lines x

main :: IO ()
main = interact runProgram
