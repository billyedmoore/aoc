import Data.Char (digitToInt, intToDigit)

readInput :: FilePath -> IO [String]
readInput fileName = fmap lines (readFile fileName)

getMaxAndArgMax :: String -> (Int, Int)
getMaxAndArgMax s =
  let (iMax, valMax, _) = foldl getMaxAndArgMaxInternal (-1, -1, 0) s
   in (iMax, valMax)
  where
    getMaxAndArgMaxInternal :: (Int, Int, Int) -> Char -> (Int, Int, Int)
    getMaxAndArgMaxInternal (iMax, valMax, currentI) x =
      let xAsInt = digitToInt x
       in if xAsInt > valMax then (currentI, xAsInt, currentI + 1) else (iMax, valMax, currentI + 1)

largestJoltageFromBank :: Int -> String -> Int
largestJoltageFromBank numToTurnOn s = read (largestJoltageFromBankInternal s numToTurnOn "")
  where
    largestJoltageFromBankInternal :: String -> Int -> String -> String
    largestJoltageFromBankInternal s numToTurnOn currentString =
      let (digitI, digitVal) = getMaxAndArgMax (take (length s - numToTurnOn + 1) s)
       in if numToTurnOn > 1
            then
              largestJoltageFromBankInternal (drop (digitI + 1) s) (numToTurnOn - 1) (currentString ++ [intToDigit digitVal])
            else currentString ++ [intToDigit digitVal]

main :: IO ()
main = do
  inputs <- readInput "three.input"
  let partOneSol = sum (map (largestJoltageFromBank 2) inputs)
  let partTwoSol = sum (map (largestJoltageFromBank 12) inputs)
  putStrLn ("Part One Solution " ++ show partOneSol)
  putStrLn ("Part Two Solution " ++ show partTwoSol)
