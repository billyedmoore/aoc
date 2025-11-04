import Data.List (inits, tails)

readLine :: [String] -> [Int]
readLine = map read

readInput :: FilePath -> IO [[Int]]
readInput fileName = fmap (map (readLine . words) . lines) (readFile fileName)

-- Convert report to a diff
getDiff :: [Int] -> [Int]
getDiff input = zipWith (-) (safeTail input) input

-- Check if a report is valid from the diff
isDiffValid :: [Int] -> Bool
isDiffValid diff =
  (all (> 0) diff || all (< 0) diff)
    && (all (\n -> abs n <= 3) diff && all (\n -> abs n >= 1) diff)

-- Tail if tail available else []
safeTail :: [Int] -> [Int]
safeTail (head : tail) = tail
safeTail [] = []

-- All possible reports using the problem dampener
allPossibleReports :: [Int] -> [[Int]]
allPossibleReports xs = zipWith (++) (inits xs) (map safeTail (tails xs))

main :: IO ()
main = do
  inputs <- readInput "two.input"

  let partOneDiffs = map getDiff inputs
  let partOneResult = length (filter isDiffValid partOneDiffs)
  putStrLn ("Part One Solution " ++ show partOneResult)

  let partTwoDiffs = map (map getDiff . allPossibleReports) inputs
  let partTwoResult = length $ filter (any isDiffValid) partTwoDiffs
  putStrLn ("Part Two Solution " ++ show partTwoResult)
