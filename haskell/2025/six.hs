import Data.List (transpose)

readInput :: FilePath -> IO [[String]]
readInput fileName = fmap (transpose . map words . reverse . lines) (readFile fileName)

handleEq :: [String] -> Int
handleEq ("+" : nums) = sum (map read nums)
handleEq ("*" : nums) = product (map read nums)
handleEq _ = error "Invlalid Eq"

main :: IO ()
main = do
  inputs <- readInput "six.input"
  let partOneSol = sum $ map handleEq inputs
  putStrLn ("Part One Solution " ++ show partOneSol)
