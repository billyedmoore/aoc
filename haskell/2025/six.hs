import Data.List (transpose)

readInput :: FilePath -> IO [[String]]
readInput fileName =
  fmap
    ( map removeSpaceFromOpLine
        . transpose
        . parseEquestions
        . reverse
        . lines
    )
    (readFile fileName)

parseEquestions :: [String] -> [[String]]
parseEquestions (opLine : rest) =
  let colWidths = getColumnWidths opLine
   in map (parseLine colWidths) (opLine : rest)

getColumnWidths :: String -> [Int]
getColumnWidths [] = []
getColumnWidths (op : rest)
  | op == '*' || op == '+' =
      let (whitespace, restAfterWhitespace) = span (== ' ') rest
       in let len = length whitespace
           in let colWidth = if null restAfterWhitespace then len + 1 else len
               in colWidth : getColumnWidths restAfterWhitespace
  | otherwise = error ("Invalid Operator: " ++ show op)

parseLine :: [Int] -> String -> [String]
parseLine (colLen : colsLensRest) s = take colLen s : parseLine colsLensRest (drop (colLen + 1) s)
parseLine [] s = []

removeSpaceFromOpLine :: [String] -> [String]
removeSpaceFromOpLine (opLine : rest) = filter (/= ' ') opLine : rest

prepEqPartTwo :: [String] -> [String]
prepEqPartTwo (op : nums) = op : (transpose . reverse) nums

handleEq :: [String] -> Int
handleEq ("+" : nums) = sum (map read nums)
handleEq ("*" : nums) = product (map read nums)
handleEq s = error ("Invalid Operator: " ++ show s)

main :: IO ()
main = do
  inputs <- readInput "six.input"
  let partOneSol = sum $ map handleEq inputs
  putStrLn ("Part One Solution " ++ show partOneSol)
  let partTwoSol = sum $ map (handleEq . prepEqPartTwo) inputs
  putStrLn ("Part Two Solution " ++ show partTwoSol)
