import Debug.Trace

firstChar :: String -> Char
firstChar (c : cs) = c
firstChar [] = error "Cannot get the first char of an empty string"

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

parseRange :: String -> (Int, Int)
parseRange s =
  let (s1, _ : s2) = break (== '-') s
   in (read s1, read s2)

parseLine :: String -> (Int, Int, Char, String)
parseLine line =
  let [rangeStr, charStrColon, password] = words line
      targetChar = firstChar charStrColon
      (lower, upper) = parseRange rangeStr
   in (lower, upper, targetChar, password)

readInput :: FilePath -> IO [(Int, Int, Char, String)]
readInput fileName = fmap (map parseLine . lines) (readFile fileName)

isValidPartOne :: (Int, Int, Char, String) -> Bool
isValidPartOne (lower, upper, char, password) =
  let n = length (filter (== char) password)
   in (n >= lower) && (n <= upper)

isValidPartTwo :: (Int, Int, Char, String) -> Bool
isValidPartTwo (lower, upper, char, password) =
  -- not good haskell practice to index like this but we know the indexes will be valid
  xor ((password !! (lower - 1)) == char) ((password !! (upper - 1)) == char)

main :: IO ()
main = do
  numbs <- readInput "two.input"
  let partOneSol = length (filter id (map isValidPartOne numbs))
  putStrLn ("Park One Solution " ++ show partOneSol)
  let partTwoSol = length (filter id (map isValidPartTwo numbs))
  putStrLn ("Park Two Solution " ++ show partTwoSol)
