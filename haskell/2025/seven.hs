import Data.Map qualified as M
import Data.Set qualified as S

data MapElem = Splitter

type PositionMap = M.Map (Int, Int) MapElem

type VisitedSet = S.Set (Int, Int)

readInput :: FilePath -> IO ((Int, Int), PositionMap, (Int, Int))
readInput fileName =
  fmap processInput (readFile fileName)
  where
    processInput :: String -> ((Int, Int), PositionMap, (Int, Int))
    processInput input =
      let inputLines = lines input
       in ((length inputLines, (length . assertHead) inputLines), inputToMap inputLines, getStartPos inputLines)

inputToMap :: [String] -> PositionMap
inputToMap inputLines =
  M.fromList
    [ ((i, j), Splitter)
    | (i, line) <- zip [0 ..] inputLines,
      (j, c) <- zip [0 ..] line,
      c == '^'
    ]

assertHead :: [a] -> a
assertHead (x : xs) = x
assertHead [] = error "Head failed because list is empty"

getStartPos :: [String] -> (Int, Int)
getStartPos (firstLine : rest) = assertHead [(0, j) | (j, c) <- zip [0 ..] firstLine, c == 'S']

-- position map -> height of the map -> currentPos -> visitedSet -> number of splitters hit,newVisited
traverseMap :: PositionMap -> (Int, Int) -> (Int, Int) -> VisitedSet -> (Int, VisitedSet)
traverseMap map (height, width) (i, j) visited =
  let visitedInclCurrent = S.insert (i, j) visited
   in case (S.member (i, j) visited, M.lookup (i, j) map) of
        (True, _) -> (0, visitedInclCurrent)
        (False, Nothing) ->
          if i < height && j > 0 && j < width
            then traverseMap map (height, width) (i + 1, j) visitedInclCurrent
            else (0, visitedInclCurrent)
        (False, Just n) ->
          let (left, leftVisited) = traverseMap map (height, width) (i + 1, j + 1) visitedInclCurrent
           in let (right, rightVisited) = traverseMap map (height, width) (i + 1, j - 1) leftVisited
               in (1 + left + right, rightVisited)

main :: IO ()
main = do
  ((inputHeight, inputWidth), inputMap, startPos) <- readInput "seven.input"
  let (partOneSol, _) = traverseMap inputMap (inputHeight, inputWidth) startPos S.empty
  putStrLn ("Part One Solution " ++ show partOneSol)
