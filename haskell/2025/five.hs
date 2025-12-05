import Data.List (sortOn)

readInput :: FilePath -> IO [String]
readInput fileName = fmap lines (readFile fileName)

splitInput :: [String] -> [String] -> ([String], [String])
splitInput ("" : xs) curr = (curr, xs)
splitInput (str : xs) curr = splitInput xs (curr ++ [str])

parseRanges :: [String] -> [(Int, Int)]
parseRanges = map (parseRange "")
  where
    parseRange :: String -> String -> (Int, Int)
    parseRange curr ('-' : xs) = (read curr, read xs)
    parseRange curr (c : xs) = parseRange (curr ++ [c]) xs

isItemFresh :: Int -> [(Int, Int)] -> Bool
isItemFresh item ranges = or [item >= lower && item <= upper | (lower, upper) <- ranges]

tailOrEmpty :: [a] -> [a]
tailOrEmpty [] = []
tailOrEmpty (x : xs) = xs

-- Head to be used when sure the list cannot be empty
headAssert :: [a] -> a
headAssert [] = error "Head called on empty list"
headAssert (x : xs) = x

-- Merge the ranges into non-colliding ranges
mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges ranges = mergeRangesInternal ranges []
  where
    mergeRangesInternal :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    mergeRangesInternal [] processed = processed
    mergeRangesInternal (currentRange : toProcess) [] = mergeRangesInternal toProcess [currentRange]
    mergeRangesInternal ((currentLower, currentUpper) : toProcess) processed =
      let (prevLower, prevUpper) = headAssert processed
       in if currentLower <= prevUpper
            then
              mergeRangesInternal toProcess ((prevLower, max currentUpper prevUpper) : tailOrEmpty processed)
            else
              mergeRangesInternal toProcess ((currentLower, currentUpper) : processed)

main :: IO ()
main = do
  inputs <- readInput "five.input"
  let (unparsedRanges, unparsedItems) = splitInput inputs []
  let ranges = mergeRanges (sortOn fst (parseRanges unparsedRanges))
  let items :: [Int]
      items = map read unparsedItems

  let partOneSol = length [item | item <- items, isItemFresh item ranges]
  let partTwoSol = sum [(upper + 1) - lower | (lower, upper) <- ranges]
  putStrLn ("Part One Solution " ++ show partOneSol)
  putStrLn ("Part Two Solution " ++ show partTwoSol)
