import Data.Char (isDigit)
import Data.Maybe (fromMaybe, isJust)

-- Returns rest of the string if valid else Nothing
isSubStr :: String -> String -> Maybe String
isSubStr "" rest = Just rest
isSubStr _ "" = Nothing
isSubStr (c : restOfTarget) (v : restOfVal) = if c == v then isSubStr restOfTarget restOfVal else Nothing

handleN :: String -> Maybe (String, Int)
handleN str = handleNInternal str ""
  where
    handleNInternal :: String -> String -> Maybe (String, Int)
    handleNInternal "" "" = Nothing
    handleNInternal "" acc = if not (null acc) then Just ("", read acc) else Nothing
    handleNInternal (c : str) acc
      | isDigit c = handleNInternal str (c : acc)
      | otherwise = if not (null acc) && (length acc <= 3) then Just (c : str, read (reverse acc)) else Nothing

-- Handle a string that may or may not be a mul
-- If mul do the mul and return the result
handleString :: String -> Int
handleString str =
  fromMaybe
    0
    ( do
        restAfterMul <- isSubStr "mul(" str
        (restAfterFirstN, firstN) <- handleN restAfterMul
        restAfterComma <- isSubStr "," restAfterFirstN
        (restAfterSecondN, secondN) <- handleN restAfterComma
        _ <- isSubStr ")" restAfterSecondN
        return (firstN * secondN)
    )

handleInputStr :: String -> Int -> Int
handleInputStr (c : str) acc =
  let res = handleString (c : str)
   in handleInputStr str (acc + res)
handleInputStr "" acc = acc

main :: IO ()
main = do
  inputs <- readFile "three.input"
  let partOneSol = handleInputStr inputs 0
  -- putStrLn ("Part One Solution " ++ show partOneSol)
  print partOneSol
