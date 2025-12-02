data Direction = Downwards | Upwards

readInput :: FilePath -> IO [String]
readInput fileName = fmap (splitStr ',') (readFile fileName)

modifyLast :: (String -> String) -> [String] -> [String]
modifyLast _ [] = []
modifyLast f [x] = [f x]
modifyLast f (x : xs) = x : modifyLast f xs

splitStr :: Char -> String -> [String]
splitStr delim = foldl splitStrStep [""]
  where
    splitStrStep :: [String] -> Char -> [String]
    splitStrStep acc '\n' = acc
    splitStrStep acc c
      | c == delim = acc ++ [""]
      | otherwise = modifyLast (\s -> s ++ [c]) acc

parsePairs :: String -> [Int]
parsePairs str = map read (splitStr '-' str)

pairsToRange :: [Int] -> [Int]
-- crash and burn if its not a pair, very idomatic I know
pairsToRange [start, end] = [start .. end]

inputsToProductsIds :: [String] -> [Int]
inputsToProductsIds = concatMap (pairsToRange . parsePairs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- factors of n but excluding n
factors :: Int -> [Int]
factors n
  | n >= 2 = [x | x <- [1 .. n - 1], n `mod` x == 0]
  | otherwise = []

allEqual :: [String] -> Bool
allEqual [] = True
-- head is safe because [] is handled
allEqual xs = all (\e -> e == head xs) xs

isInvalidWithNRepetitions :: String -> Int -> Bool
isInvalidWithNRepetitions s n =
  let chunks = chunksOf n s
   in allEqual chunks

isInvalidInPartOne :: Int -> Bool
isInvalidInPartOne i =
  let iAsStr = show i
   in let len = length iAsStr
       in let frt = take (len `div` 2) iAsStr
           in let bck = drop (len `div` 2) iAsStr
               in even len && frt == bck

isInvalidInPartTwo :: Int -> Bool
isInvalidInPartTwo i =
  let iAsStr = show i
   in or [isInvalidWithNRepetitions iAsStr n | n <- factors (length iAsStr)]

main :: IO ()
main = do
  inputs <- readInput "two.input"
  let partOneSol = sum (filter isInvalidInPartOne (inputsToProductsIds inputs))
  let partTwoSol = sum (filter isInvalidInPartTwo (inputsToProductsIds inputs))
  putStrLn ("Part One Solution " ++ show partOneSol)
  putStrLn ("Part Two Solution " ++ show partTwoSol)
