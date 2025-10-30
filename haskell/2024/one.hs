import Data.List (sort)

readLine :: [String] -> (Int, Int)
readLine [str1, str2] = (read str1, read str2)
readLine _ = error "Input line should have 2 words"

readInput :: FilePath -> IO [(Int, Int)]
readInput fileName = fmap (map (readLine . words) . lines) (readFile fileName)

main :: IO ()
main = do
  inputs <- readInput "one.input"
  let (listOne, listTwo) = unzip inputs
  let sortedListOne = sort listOne
  let sortedListTwo = sort listTwo

  let partOneResult = sum (zipWith (\a b -> abs (a - b)) sortedListOne sortedListTwo)

  putStrLn ("Park One Solution " ++ show partOneResult)

  let partTwoResult = sum (map (\a -> a * length (filter (== a) sortedListTwo)) sortedListOne)
  putStrLn ("Park Two Solution " ++ show partTwoResult)
