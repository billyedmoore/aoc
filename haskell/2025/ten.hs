import Data.List qualified as L
import Debug.Trace (trace)

readInput :: FilePath -> IO [([Bool], [[Bool]], [Int])]
readInput fileName = fmap (map (parseLine . words) . lines) (readFile fileName)

parseLine :: [String] -> ([Bool], [[Bool]], [Int])
parseLine (target : rest) = (parseLightTargets target, map parseButton (init rest), parseBrackettedInts (last rest) "")
parseLine [] = error "Invalid Line"

parseLightTargets :: String -> [Bool]
parseLightTargets ('[' : rest) = parseLightTargets rest
parseLightTargets ('.' : rest) = False : parseLightTargets rest
parseLightTargets ('#' : rest) = True : parseLightTargets rest
parseLightTargets (']' : rest) = []
parseLightTargets [] = error "Unexpected case"

parseBrackettedInts :: String -> String -> [Int]
parseBrackettedInts (c : rest) nAcc
  | c == '(' || c == '{' = parseBrackettedInts rest nAcc
  | c == ',' || c == ')' || c == '}' =
      if not (null nAcc)
        then read (reverse nAcc) : parseBrackettedInts rest ""
        else parseBrackettedInts rest ""
  | otherwise = parseBrackettedInts rest (c : nAcc)
parseBrackettedInts v n = []

parseButton :: String -> [Bool]
parseButton s = intsToBitBoard $ parseBrackettedInts s ""
  where
    intsToBitBoard :: [Int] -> [Bool]
    intsToBitBoard xs = [i `elem` xs | i <- [0 .. (maximum xs)]]

solveLinePartOne :: ([Bool], [[Bool]]) -> Int
solveLinePartOne (target, allButtons) =
  minimum
    [ length buttons
    | buttons <- L.subsequences allButtons,
      simulateButtonPressesPartOne (length target) buttons == target
    ]

combinationsWithRepetition :: Int -> [a] -> [[a]]
combinationsWithRepetition 0 _ = [[]]
combinationsWithRepetition _ [] = []
combinationsWithRepetition n (x : xs)
  | n < 0 = []
  | otherwise = withHead ++ withoutHead
  where
    withHead = map (x :) (combinationsWithRepetition (n - 1) (x : xs))
    withoutHead = combinationsWithRepetition n xs

simulateButtonPressesPartOne :: Int -> [[Bool]] -> [Bool]
simulateButtonPressesPartOne len buttons =
  let paddedButtons = map (\b -> take len (b ++ repeat False)) buttons
   in map (foldl1 (/=)) (L.transpose paddedButtons)

simulateButtonPressesPartTwo :: Int -> [[Bool]] -> [Int]
simulateButtonPressesPartTwo len buttons =
  let paddedButtons = map (\b -> take len (b ++ repeat False)) buttons
   in let intButtons = map (\b -> [if v then 1 else 0 | v <- b]) paddedButtons
       in map sum (L.transpose intButtons)

tryNpresses :: [Int] -> Int -> [[Bool]] -> Bool
tryNpresses target n buttons = any (\b -> simulateButtonPressesPartTwo (length target) b == target) (combinationsWithRepetition n buttons)

solveLinePartTwo :: ([Int], [[Bool]]) -> Int
solveLinePartTwo (target, allButtons) = solveLinePartTwoInternal target allButtons 1
  where
    solveLinePartTwoInternal :: [Int] -> [[Bool]] -> Int -> Int
    solveLinePartTwoInternal target allButtons n =
      if tryNpresses target n allButtons
        then trace ("Line " ++ show target ++ " Success  " ++ show n) n
        else solveLinePartTwoInternal target allButtons (n + 1)

main :: IO ()
main = do
  inputs <- readInput "ten.input"

  let partOneSol = sum (map (\(lights, buttons, _) -> solveLinePartOne (lights, buttons)) inputs)
  let partTwoSol = sum (map (\(_, buttons, target) -> solveLinePartTwo (target, buttons)) inputs)

  putStrLn ("Part One Solution " ++ show partOneSol)
  putStrLn ("Part Two Solution " ++ show partTwoSol)
