{-
 - I'm sure there are more idomatic/satifiying ways to
 - do this but it works lol.
 -}

import Data.Map qualified as M
import Data.Set qualified as S

type PositionSet = S.Set (Int, Int)

type HistoryMap = M.Map (Int, Int) Int

readInput :: FilePath -> IO ((Int, Int), PositionSet, (Int, Int))
readInput fileName =
  fmap processInput (readFile fileName)
  where
    processInput :: String -> ((Int, Int), PositionSet, (Int, Int))
    processInput input =
      let inputLines = lines input
       in ((length inputLines, (length . assertHead) inputLines), inputToMap inputLines, getStartPos inputLines)

inputToMap :: [String] -> PositionSet
inputToMap inputLines =
  S.fromList
    [ (i, j)
    | (i, line) <- zip [0 ..] inputLines,
      (j, c) <- zip [0 ..] line,
      c == '^'
    ]

assertHead :: [a] -> a
assertHead (x : xs) = x
assertHead [] = error "Head failed because list is empty"

getStartPos :: [String] -> (Int, Int)
getStartPos (firstLine : rest) = assertHead [(0, j) | (j, c) <- zip [0 ..] firstLine, c == 'S']

-- position map -> (height,width) of map -> currentPos -> visitedSet -> number of splitters hit,newVisited
partOne :: PositionSet -> (Int, Int) -> (Int, Int) -> PositionSet -> (Int, PositionSet)
partOne map (height, width) (i, j) visited =
  let visitedInclCurrent = S.insert (i, j) visited
   in case (S.member (i, j) visited, S.member (i, j) map) of
        (True, _) -> (0, visitedInclCurrent)
        (False, False) ->
          if i < height && j > 0 && j < width
            then partOne map (height, width) (i + 1, j) visitedInclCurrent
            else (0, visitedInclCurrent)
        (False, True) ->
          let (left, leftVisited) = partOne map (height, width) (i + 1, j + 1) visitedInclCurrent
           in let (right, rightVisited) = partOne map (height, width) (i + 1, j - 1) leftVisited
               in (1 + left + right, rightVisited)

-- position map -> (height,width) of the map -> currentPos -> history -> (numTimelines,newHistory)
partTwo :: PositionSet -> (Int, Int) -> (Int, Int) -> HistoryMap -> (Int, HistoryMap)
partTwo map (height, width) (i, j) history = case (M.lookup (i, j) history, S.member (i, j) map) of
  (Just n, _) -> (n, history)
  (Nothing, False) ->
    if i < height && j > 0 && j < width
      then partTwo map (height, width) (i + 1, j) history
      else (1, history)
  (Nothing, True) ->
    let (left, leftHist) = partTwo map (height, width) (i + 1, j + 1) history
     in let (right, rightHist) = partTwo map (height, width) (i + 1, j - 1) (M.insert (i + 1, j + 1) left leftHist)
         in (left + right, rightHist)

main :: IO ()
main = do
  ((inputHeight, inputWidth), inputMap, startPos) <- readInput "seven.input"
  let (partOneSol, _) = partOne inputMap (inputHeight, inputWidth) startPos S.empty
  putStrLn ("Part One Solution " ++ show partOneSol)
  let (partTwoSol, _) = partTwo inputMap (inputHeight, inputWidth) startPos M.empty
  putStrLn ("Part One Solution " ++ show partTwoSol)
