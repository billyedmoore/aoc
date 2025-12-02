import Text.Parsec (parse)

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
-- crash and burn otherwise -> ,idomatic I know
pairsToRange [start, end] = [start .. end]

inputsToProductsIds :: [String] -> [Int]
inputsToProductsIds = concatMap (pairsToRange . parsePairs)

isInvalidInPartOne :: Int -> Bool
isInvalidInPartOne i =
  let iAsStr = show i
   in let len = length iAsStr
       in let frt = take (len `div` 2) iAsStr
           in let bck = drop (len `div` 2) iAsStr
               in even len && frt == bck

main :: IO ()
main = do
  inputs <- readInput "two.input"
  let partOneSol = sum (filter isInvalidInPartOne (inputsToProductsIds inputs))
  putStrLn ("Park One Solution " ++ show partOneSol)
