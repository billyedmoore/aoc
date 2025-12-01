import Debug.Trace

data Direction = Downwards | Upwards

readInput :: FilePath -> IO [String]
readInput fileName = fmap lines (readFile fileName)

updatePos :: (Int, Int) -> Int -> (Int -> Int -> Int) -> (Int, Int)
updatePos (currentPos, zeroCount) amount f =
  let newPos = (currentPos `f` amount) `mod` 100
   in trace
        ("Pos changed " ++ show amount ++ " from " ++ show currentPos ++ " to " ++ show newPos)
        (newPos, if newPos == 0 then zeroCount + 1 else zeroCount)

handleInstruction :: (Int, Int) -> String -> (Int, Int)
handleInstruction acc ('R' : numStr) = updatePos acc (read numStr) (+)
handleInstruction acc ('L' : numStr) = updatePos acc (read numStr) (-)
handleInstruction acc s = trace ("Received string in unexpected format (" ++ s ++ "), ignoring") acc

main :: IO ()
main = do
  inputs <- readInput "one.input"
  let partOneSol = foldl handleInstruction (50, 0) inputs
  putStrLn ("Park One Solution " ++ show partOneSol)
