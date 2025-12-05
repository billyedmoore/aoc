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

main :: IO ()
main = do
  inputs <- readInput "five.input"
  let (unparsedRanges, unparsedItems) = splitInput inputs []
  let ranges = parseRanges unparsedRanges
  let items :: [Int]
      items = map read unparsedItems

  let partOneSol = length [item | item <- items, isItemFresh item ranges]
  putStrLn ("Part One Solution " ++ show partOneSol)
