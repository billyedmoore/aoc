import Control.Monad (forM_, mapM, sequence)
import Data.List qualified as L
import Data.SBV
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

simulateButtonPressesPartOne :: Int -> [[Bool]] -> [Bool]
simulateButtonPressesPartOne len buttons =
  let paddedButtons = map (\b -> take len (b ++ repeat False)) buttons
   in map (foldl1 (/=)) (L.transpose paddedButtons)

minimumPresses :: ([[Bool]], [Int]) -> IO (Maybe Int)
minimumPresses (unpaddedMatrix, target) = do
  let buttons = map (\b -> take (length target) (b ++ repeat False)) unpaddedMatrix
  let numButtons = length buttons
  let names = ["p" ++ show i | i <- [0 .. numButtons - 1]]

  result <- optimize Lexicographic $ do
    presses <- mapM sInteger names

    forM_ presses $ \p -> constrain (p .>= 0)

    let logicalRows = L.transpose buttons

    forM_ (zip logicalRows target) $ \(rowFlags, targetVal) -> do
      let activeTerms = [p | (isActive, p) <- zip rowFlags presses, isActive]

      constrain $ sum activeTerms .== literal (fromIntegral targetVal)

    minimize "total" (sum presses)

  case result of
    LexicographicResult model -> do
      let maybeValues = map (`getModelValue` model) names

      case sequence maybeValues of
        Just values ->
          return (Just (fromIntegral (sum values)))
        Nothing ->
          return Nothing
    _ -> return Nothing

solveAll :: [([[Bool]], [Int])] -> IO (Maybe Int)
solveAll problems = do
  results <- mapM minimumPresses problems
  print results

  case sequence results of
    Just totals -> return (Just (sum totals))
    Nothing -> return Nothing

main :: IO ()
main = do
  inputs <- readInput "ten.input"

  let partOneSol = sum (map (\(lights, buttons, _) -> solveLinePartOne (lights, buttons)) inputs)

  let partTwoInput = map (\(_, b, i) -> (b, i)) inputs

  grandTotal <-
    solveAll
      inputList

  case grandTotal of
    Just total -> putStrLn $ "Part Two Solution " ++ show total
    Nothing -> putStrLn "Part Two Failed :("

  putStrLn ("Part One Solution " ++ show partOneSol)
