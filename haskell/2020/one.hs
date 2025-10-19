readInput :: FilePath -> IO [Int]
readInput fileName = fmap (map read . lines) (readFile fileName)

getSolutionFromCombinationsPartOne :: [(Int, Int)] -> Int
getSolutionFromCombinationsPartOne [(x, y)] = x * y
getSolutionFromCombinationsPartOne _ = -1

getSolutionFromCombinationsPartTwo :: [(Int, Int, Int)] -> Int
getSolutionFromCombinationsPartTwo [(x, y, z)] = x * y * z
getSolutionFromCombinationsPartTwo _ = -1

main :: IO ()
main = do
  numbs <- readInput "one.input"
  let partOne = [(n1, n2) | n1 <- numbs, n2 <- numbs, n1 < n2, n1 + n2 == 2020]
  let partOneSol = getSolutionFromCombinationsPartOne partOne
  putStrLn ("Park One Solution " ++ show partOneSol)
  let partTwo = [(n1, n2, n3) | n1 <- numbs, n2 <- numbs, n1 < n2, n3 <- numbs, n2 < n3, (n1 + n2 + n3) == 2020]
  let partTwoSol = getSolutionFromCombinationsPartTwo partTwo
  putStrLn ("Park Two Solution " ++ show partTwoSol)
