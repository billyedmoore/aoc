-- Neither of these are truly O(n) but they are fine
-- for this problem
import Data.Map.Strict qualified as M
import Data.Set qualified as S

data Location = Roll | Empty deriving (Show)

readInput :: FilePath -> IO [String]
readInput fileName = fmap lines (readFile fileName)

type WarehouseMap = M.Map (Int, Int) Location

prepForHashMap :: [String] -> [((Int, Int), Location)]
prepForHashMap lines =
  [ ((i, j), parseChar char)
  | (i, line) <- zip [0 ..] lines,
    (j, char) <- zip [0 ..] line
  ]

mapFromLines :: [String] -> WarehouseMap
mapFromLines lines = M.fromList (prepForHashMap lines)

parseChar :: Char -> Location
parseChar '.' = Empty
parseChar '@' = Roll
parseChar c = error ("Unexpected Char " ++ show c)

isPosRoll :: Maybe Location -> Bool
isPosRoll (Just Roll) = True
isPosRoll _ = False

handlePos :: (Int, Int) -> WarehouseMap -> Bool
handlePos (i, j) map =
  let rollList =
        [ isPosRoll (M.lookup (i + i_change, j + j_change) map)
        | i_change <- [-1 .. 1],
          j_change <- [-1 .. 1],
          i_change /= 0 || j_change /= 0
        ]
   in let count = length (filter id rollList)
       in count < 4

getMovedRolls :: WarehouseMap -> [(Int, Int)]
getMovedRolls map = [(i, j) | (i, j) <- M.keys map, isPosRoll (M.lookup (i, j) map), handlePos (i, j) map]

solvePartOne :: WarehouseMap -> Int
solvePartOne map = length (getMovedRolls map)

solvePartTwo :: WarehouseMap -> Int
solvePartTwo map = solvePartTwoInternal map 0
  where
    solvePartTwoInternal :: WarehouseMap -> Int -> Int
    solvePartTwoInternal map currentCount =
      let movedRolls = S.fromList (getMovedRolls map)
       in let newMap = M.fromList [((i, j), v) | ((i, j), v) <- M.toList map, not (S.member (i, j) movedRolls)]
           in if not (null movedRolls) then solvePartTwoInternal newMap (currentCount + length movedRolls) else currentCount

main :: IO ()
main = do
  inputs <- readInput "four.input"
  let map = mapFromLines inputs
  let partOneSol = solvePartOne map
  let partTwoSol = solvePartTwo map
  putStrLn ("Part One Solution " ++ show partOneSol)
  putStrLn ("Part Two Solution " ++ show partTwoSol)
