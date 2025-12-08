import Data.List qualified as L
import Data.Ord
import Data.Set qualified as S

type Coord = (Int, Int, Int)

readInput :: FilePath -> IO ([(Coord, Coord)], Int)
readInput fileName = fmap processInput (readFile fileName)
  where
    processInput :: String -> ([(Coord, Coord)], Int)
    processInput s =
      let inputLines = lines s
       in ((getPairs . map parseCoords) inputLines, length inputLines)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn c s = splitOnInternal c s []
  where
    splitOnInternal :: (Eq a) => a -> [a] -> [a] -> [[a]]
    splitOnInternal target (c : s) acc = case (c == target, acc) of
      (True, []) -> splitOnInternal target s []
      (True, acc) -> reverse acc : splitOnInternal target s []
      (False, acc) -> splitOnInternal target s (c : acc)
    splitOnInternal target [] acc = [reverse acc | not (null acc)]

parseCoords :: String -> Coord
parseCoords s = case splitOn ',' s of
  [a, b, c] -> (read a, read b, read c)
  _ -> error ("Invalid input line " ++ show s)

getDistance :: (Coord, Coord) -> Double
getDistance ((x1, y1, z1), (x2, y2, z2)) =
  sqrt
    ( fromIntegral (x2 - x1) ** 2
        + fromIntegral (y2 - y1) ** 2
        + fromIntegral (z2 - z1) ** 2
    )

getPairs :: [Coord] -> [(Coord, Coord)]
getPairs cs =
  L.sortBy
    (\p1 p2 -> compare (getDistance p1) (getDistance p2))
    [(c1, c2) | (c1 : rest) <- L.tails cs, c2 <- rest]

buildCircuitsForPartOne :: [(Coord, Coord)] -> [S.Set Coord] -> [S.Set Coord]
buildCircuitsForPartOne ((p1, p2) : remainingPairs) circuits = case (L.find (S.member p1) circuits, L.find (S.member p2) circuits) of
  (Just set1, Just set2) ->
    if set1 == set2
      then buildCircuitsForPartOne remainingPairs circuits
      else buildCircuitsForPartOne remainingPairs (S.union set1 set2 : L.delete set1 (L.delete set2 circuits))
  (Nothing, Just set2) -> buildCircuitsForPartOne remainingPairs (S.insert p1 set2 : L.delete set2 circuits)
  (Just set1, Nothing) -> buildCircuitsForPartOne remainingPairs (S.insert p2 set1 : L.delete set1 circuits)
  (Nothing, Nothing) -> buildCircuitsForPartOne remainingPairs (S.fromList [p1, p2] : circuits)
buildCircuitsForPartOne [] circuits = circuits

partTwo :: Int -> [(Coord, Coord)] -> [S.Set Coord] -> Int
partTwo n [] circuits = error "End Not Found"
partTwo n ((p1, p2) : remainingPairs) circuits = case (L.find (S.member p1) circuits, L.find (S.member p2) circuits) of
  (Just set1, Just set2) ->
    if set1 == set2
      then partTwo n remainingPairs circuits
      else recurseIfNotFinished n remainingPairs (S.union set1 set2 : L.delete set1 (L.delete set2 circuits)) (p1, p2)
  (Nothing, Just set2) -> recurseIfNotFinished n remainingPairs (S.insert p1 set2 : L.delete set2 circuits) (p1, p2)
  (Just set1, Nothing) -> recurseIfNotFinished n remainingPairs (S.insert p2 set1 : L.delete set1 circuits) (p1, p2)
  (Nothing, Nothing) -> recurseIfNotFinished n remainingPairs (S.fromList [p1, p2] : circuits) (p1, p2)
  where
    recurseIfNotFinished :: Int -> [(Coord, Coord)] -> [S.Set Coord] -> (Coord, Coord) -> Int
    recurseIfNotFinished n restOfPairs newCircuits (p1, p2) = case newCircuits of
      [circuit] ->
        if length circuit == n
          then (let (x1, _, _) = p1 in let (x2, _, _) = p2 in x1 * x2)
          else partTwo n restOfPairs newCircuits
      _ -> partTwo n restOfPairs newCircuits

main :: IO ()
main = do
  (pairs, nBoxes) <- readInput "eight.input"
  let topNpairs = take 1000 pairs
  let circuitLengths = map length (buildCircuitsForPartOne topNpairs [])
  let sortedCircuitLengths = L.sortBy (comparing Data.Ord.Down) circuitLengths

  let partOneSol = product (take 3 sortedCircuitLengths)
  putStrLn ("Part One Solution " ++ show partOneSol)

  let partTwoSol = partTwo nBoxes pairs []
  putStrLn ("Part Two Solution " ++ show partTwoSol)
