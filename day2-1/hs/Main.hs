import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

initializeMemory :: PrimMonad m => [Integer] -> MV.MVector (PrimState m) Integer -> m Integer
initializeMemory = V.unsafeThaw . V.fromList

addNewline :: String -> String
addNewline x = x ++ "\n"

runProgram :: String -> String
runProgram x = addNewline $ show $ V.length $ initializeMemory $ map read $ lines x

main :: IO ()
main = interact runProgram

