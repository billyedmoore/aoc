import Data.List (sort)

readLine :: [String] -> [Int]
readLine = map read

readInput :: FilePath -> IO [[Int]]
readInput fileName = fmap (map (readLine . words) . lines) (readFile fileName)

isDiffValid :: [Int] -> Bool
isDiffValid diff =
  (all (> 0) diff || all (< 0) diff)
    && (all (\n -> abs n <= 3) diff && all (\n -> abs n >= 1) diff)

safeTail :: [Int] -> [Int]
safeTail (head : tail) = tail
safeTail [] = []

main :: IO ()
main = do
  inputs <- readInput "two.input"

  let diffs = map (\input -> zipWith (-) (safeTail input) input) inputs
  let result = length (filter isDiffValid diffs)
  putStrLn ("Part One Solution " ++ show result)
