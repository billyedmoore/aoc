import Data.List qualified as L
import Data.Ord
import Data.Set qualified as S

type Coord = (Int, Int, Int)

readInput :: FilePath -> IO [(Coord, Coord)]
readInput fileName = fmap (getPairs . map parseCoords . lines) (readFile fileName)

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

buildCircuits :: [(Coord, Coord)] -> [S.Set Coord] -> [S.Set Coord]
buildCircuits ((p1, p2) : pairsOther) circuits = case (L.find (S.member p1) circuits, L.find (S.member p2) circuits) of
  (Just set1, Just set2) ->
    if set1 == set2
      then buildCircuits pairsOther circuits
      else buildCircuits pairsOther (S.union set1 set2 : L.delete set1 (L.delete set2 circuits))
  (Nothing, Just set2) -> buildCircuits pairsOther (S.insert p1 set2 : L.delete set2 circuits)
  (Just set1, Nothing) -> buildCircuits pairsOther (S.insert p2 set1 : L.delete set1 circuits)
  (Nothing, Nothing) -> buildCircuits pairsOther (S.fromList [p1, p2] : circuits)
buildCircuits [] circuits = circuits

main :: IO ()
main = do
  pairs <- readInput "eight.input"
  let topNpairs = take 1000 pairs
  let circuitLengths = map length (buildCircuits topNpairs [])
  let sortedCircuitLengths = L.sortBy (comparing Data.Ord.Down) circuitLengths
  let partOneSol = product (take 3 sortedCircuitLengths)

  putStrLn ("Part One Solution " ++ show partOneSol)
