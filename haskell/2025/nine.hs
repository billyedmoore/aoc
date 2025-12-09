{-
 - Part 2 was hard lol, figured out pretty quickly that it needed to
 -  determine if each rectangle was in the allowable polygon but
 -  had frankly no clue how to do that. Much was learn't.
 - -}

import Data.List qualified as L

type Coord = (Int, Int)

-- Polygon as a list of edges
type Edge = (Coord, Coord)

type Polygon = [Edge]

readInput :: FilePath -> IO [Coord]
readInput fileName = fmap (parseCoords . lines) (readFile fileName)

parseCoords :: [String] -> [(Int, Int)]
parseCoords = map (parseCoord "")
  where
    parseCoord :: String -> String -> (Int, Int)
    parseCoord curr (',' : xs) = (read (reverse curr), read xs)
    parseCoord curr (c : xs) = parseCoord (c : curr) xs

-- Get the area of a rect from two opposite corners
rectArea :: (Coord, Coord) -> Int
rectArea ((x1, y1), (x2, y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

getPairs :: [Coord] -> [(Coord, Coord)]
getPairs cs = [(c1, c2) | (c1 : rest) <- L.tails cs, c2 <- rest]

isPointInPolygon :: Polygon -> Coord -> Bool
isPointInPolygon poly (x, y) =
  any pointTouchesEdge poly || odd (length [True | edge <- poly, doesRayIntersect edge])
  where
    -- If touching an edge its in, its too confusing to ray-trace this
    pointTouchesEdge :: Edge -> Bool
    pointTouchesEdge ((x1, y1), (x2, y2))
      | x1 == x2 = x == x1 && y >= min y1 y2 && y <= max y1 y2
      | y1 == y2 = y == y1 && x >= min x1 x2 && x <= max x1 x2
      | otherwise = False

    -- If ray intersects with odd number of edges then the point is in
    doesRayIntersect :: Edge -> Bool
    doesRayIntersect ((x1, y1), (x2, y2))
      | y1 == y2 = (y >= y1) && (x >= min x1 x2 && x < max x1 x2)
      | otherwise = False

doPolyEdgesIntersectRect :: Polygon -> Polygon -> Bool
doPolyEdgesIntersectRect rect poly =
  let points = [p | (p1, p2) <- rect, p <- [p1, p2]]
   in let bounds = (minimum (map fst points), maximum (map fst points), minimum (map snd points), maximum (map snd points))
       in or [True | edge <- poly, doesEdgeIntersectRect bounds edge]
  where
    doesEdgeIntersectRect :: (Int, Int, Int, Int) -> Edge -> Bool
    doesEdgeIntersectRect (xMin, xMax, yMin, yMax) edge = case edge of
      ((x1, y1), (x2, y2))
        | x1 == x2 -> (x1 > xMin && x1 < xMax) && (min y1 y2 < yMax) && (max y1 y2 > yMin)
      ((x1, y1), (x2, y2))
        | y1 == y2 -> (y1 > yMin && y1 < yMax) && (min x1 x2 < xMax) && (max x1 x2 > xMin)
      _ -> error "Polygon must only have horizontal or vertical lines."

verticesToPolygon :: [Coord] -> Polygon
verticesToPolygon vs = zip vs (drop 1 (cycle vs))

-- Rect from diagonally opposing corners to a list of edges
cornerPairsToPoly :: (Coord, Coord) -> Polygon
cornerPairsToPoly ((x1, y1), (x2, y2)) = verticesToPolygon [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]

isRectValid :: Polygon -> (Coord, Coord) -> Bool
isRectValid poly corners =
  let rect = cornerPairsToPoly corners
   in all (isPointInPolygon poly . fst) rect && not (doPolyEdgesIntersectRect rect poly)

main :: IO ()
main = do
  redTileCoords <- readInput "nine.input"
  let partOneSol = maximum (map rectArea (getPairs redTileCoords))

  putStrLn ("Part One Solution " ++ show partOneSol)
  let poly = verticesToPolygon redTileCoords
  let partTwoSol = maximum (map rectArea (filter (isRectValid poly) (getPairs redTileCoords)))
  putStrLn ("Part Two Solution " ++ show partTwoSol)
