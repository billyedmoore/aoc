import Data.List qualified as L

type Coord = (Int, Int)

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

main :: IO ()
main = do
  redTileCoords <- readInput "nine.input"
  let partOneSol = maximum (map rectArea (getPairs redTileCoords))

  putStrLn ("Part One Solution " ++ show partOneSol)
