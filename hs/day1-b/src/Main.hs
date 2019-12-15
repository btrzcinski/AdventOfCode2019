fuelRequired :: Int -> Int
fuelRequired mass
    | fuel < 0  = 0
    | otherwise = fuel + (fuelRequired fuel)
  where
    fuel = mass `div` 3 - 2

fuelForModules :: [Int] -> Int
fuelForModules x = sum $ map fuelRequired x

addNewline :: String -> String
addNewline x = x ++ "\n"

getMasses :: String -> String
getMasses x = addNewline $ show $ fuelForModules $ map read $ lines x

main :: IO ()
main = interact getMasses

