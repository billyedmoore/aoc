data Direction = Downwards | Upwards

readInput :: FilePath -> IO [String]
readInput fileName = fmap lines (readFile fileName)

zeroPointingCount :: Int -> Int -> Int
zeroPointingCount startPos moveAmount
  | moveAmount == 0 = 0
  | moveAmount > 0 =
      let distToNextZero = 100 - (startPos `mod` 100)
       in if moveAmount >= distToNextZero
            then 1 + (moveAmount - distToNextZero) `div` 100
            else 0
  | moveAmount < 0 =
      -- This case should not have broken my brain as much as it did
      let distToNextZero = if startPos == 0 then 100 else startPos `mod` 100
       in if abs moveAmount >= distToNextZero
            then 1 + (abs moveAmount - distToNextZero) `div` 100
            else 0

updatePos :: (Int, Int, Int) -> Int -> (Int, Int, Int)
updatePos (currentPos, partOneCount, partTwoCount) amount =
  let newPos = (currentPos + amount) `mod` 100
   in let zeroCount = zeroPointingCount currentPos amount
       in ( newPos,
            if newPos == 0 then partOneCount + 1 else partOneCount,
            partTwoCount + zeroCount
          )

handleInstruction :: (Int, Int, Int) -> String -> (Int, Int, Int)
handleInstruction acc ('R' : numStr) = updatePos acc (read numStr)
handleInstruction acc ('L' : numStr) = updatePos acc (negate (read numStr))

main :: IO ()
main = do
  inputs <- readInput "one.input"
  let (_, partOneSol, partTwoSol) = foldl handleInstruction (50, 0, 0) inputs
  putStrLn ("Park One Solution " ++ show partOneSol)
  putStrLn ("Park Two Solution " ++ show partTwoSol)
