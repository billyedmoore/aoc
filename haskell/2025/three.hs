import Data.Char (digitToInt)

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

largestJoltageFromBank :: String -> Int
largestJoltageFromBank s =
  let (firstDigitI, firstDigitVal) = getMaxAndArgMax (take (length s - 1) s)
   in let (secondDigitI, secondDigitVal) = getMaxAndArgMax (drop (firstDigitI + 1) s)
       in firstDigitVal * 10 + secondDigitVal

main :: IO ()
main = do
  inputs <- readInput "three.input"
  let partOneSol = sum (map largestJoltageFromBank inputs)
  putStrLn ("Part One Solution " ++ show partOneSol)
