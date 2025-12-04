import Data.Map.Strict as M

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

foundInMap :: Maybe Location -> Bool
foundInMap (Just Roll) = True
foundInMap _ = False

handlePos :: (Int, Int) -> WarehouseMap -> Bool
handlePos (i, j) map =
  let isCurrentPosRoll = foundInMap (M.lookup (i, j) map)
   in let rollList =
            if not isCurrentPosRoll
              then []
              else
                [ foundInMap (M.lookup (i + i_change, j + j_change) map)
                | i_change <- [-1 .. 1],
                  j_change <- [-1 .. 1],
                  i_change /= 0 || j_change /= 0
                ]
       in let count = length (Prelude.filter id rollList)
           in count < 4 && isCurrentPosRoll

solvePartOne :: WarehouseMap -> Int
solvePartOne map = length (Prelude.filter id [handlePos (i, j) map | (i, j) <- M.keys map])

main :: IO ()
main = do
  inputs <- readInput "four.input"
  let map = mapFromLines inputs
  let partOneSol = solvePartOne map
  putStrLn ("Part One Solution " ++ show partOneSol)
