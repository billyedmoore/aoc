import Data.Map qualified as M

type CrossWord = M.Map (Int, Int) Char

readInput :: String -> IO CrossWord
readInput filename = fmap (prepareMap . lines) (readFile filename)

prepareMap :: [String] -> CrossWord
prepareMap inputLines =
  M.fromList
    [ ((i, j), c)
    | (i, line) <- zip [0 ..] inputLines,
      (j, c) <- zip [0 ..] line
    ]

listToNestedTuple :: [(Int, Int)] -> ((Int, Int), (Int, Int), (Int, Int))
listToNestedTuple [a, b, c] = (a, b, c)
listToNestedTuple s = error "[(a,b),(c,d),(e,f)]-> ((a,b),(c,d),(e,f)) only works on lists of length 3."

-- possibleXmasses :: [((Int, Int), (Int, Int), (Int, Int))]
possibleXmasses =
  [ listToNestedTuple [(i, 0) | i <- [1 .. 3]],
    listToNestedTuple [(0, i) | i <- [1 .. 3]],
    listToNestedTuple [(i, i) | i <- [1 .. 3]],
    listToNestedTuple [(i, -i) | i <- [1 .. 3]],
    listToNestedTuple [(-i, i) | i <- [1 .. 3]],
    listToNestedTuple [(i, i) | i <- reverse [-3 .. -1]],
    listToNestedTuple [(i, 0) | i <- reverse [-3 .. -1]],
    listToNestedTuple [(0, i) | i <- reverse [-3 .. -1]]
  ]

isValidXmas :: (Int, Int) -> CrossWord -> ((Int, Int), (Int, Int), (Int, Int)) -> Bool
isValidXmas (i, j) crossword ((mi, mj), (ai, aj), (si, sj)) =
  case (M.lookup (i + mi, j + mj) crossword, M.lookup (i + ai, j + aj) crossword, M.lookup (i + si, j + sj) crossword) of
    (Just 'M', Just 'A', Just 'S') -> True
    (m, a, s) -> False

isValidMasX :: (Int, Int) -> CrossWord -> Bool
isValidMasX (i, j) crossword =
  -- (top left, bottom right, top right, bottom left)
  case ( M.lookup (i + 1, j - 1) crossword,
         M.lookup (i - 1, j + 1) crossword,
         M.lookup (i + 1, j + 1) crossword,
         M.lookup (i - 1, j - 1) crossword
       ) of
    (Just 'S', Just 'M', Just 'S', Just 'M') -> True
    (Just 'M', Just 'S', Just 'S', Just 'M') -> True
    (Just 'S', Just 'M', Just 'M', Just 'S') -> True
    (Just 'M', Just 'S', Just 'M', Just 'S') -> True
    (_, _, _, _) -> False

solvePositionForPartOne :: CrossWord -> (Int, Int) -> Int
solvePositionForPartOne crossword coord = case M.lookup coord crossword of
  (Just 'X') -> length [v | v <- map (isValidXmas coord crossword) possibleXmasses, v]
  _ -> 0

solvePositionForPartTwo :: CrossWord -> (Int, Int) -> Bool
solvePositionForPartTwo crossword coord = case M.lookup coord crossword of
  (Just 'A') -> isValidMasX coord crossword
  _ -> False

main :: IO ()
main = do
  inputs <- readInput "four.input"
  let partOneSol = sum (map (solvePositionForPartOne inputs) (M.keys inputs))
  putStrLn ("Part One Solution " ++ show partOneSol)
  let partTwoSol = length [e | e <- map (solvePositionForPartTwo inputs) (M.keys inputs), e]
  putStrLn ("Part Two Solution " ++ show partTwoSol)
