module Src.Lib.InfixToPost where

import           Data.Char
import           Data.List

precedence = [("+", 1), ("-", 1), ("*", 2), ("/", 2), ("^", 3), ("(", 4)]

infixToPost :: String -> String
infixToPost xs = unwords (convert (map (\x -> [x]) xs) []) where
    convert inp st
      | inp == [] && st == "" = []
      | inp == [] = [[head st]] ++ convert inp (tail st)
      | all (== True) (map isNumber (head inp)) =
        head inp : convert (tail inp) st
      | head inp == "(" = 
        convert (tail inp) (head (head inp) : st)
      | head inp == ")" =
        map (\x -> [x]) (takeWhile (/= '(') st) ++ convert (tail inp) (tail (dropWhile (/= '(') st))
      | st == "" || ((head inp `isSubsequenceOf` "*/+-^") && head st == '(') =
        convert (tail inp) (head (head inp) : st)
      | st == "" ||((head inp `isSubsequenceOf` "*/+-^") && checkPrec (head inp) [(head st)]) =
        convert (tail inp) (head (head inp) : st)
      | (head inp `isSubsequenceOf` "*/+-^") && not (checkPrec (head inp) [(head st)]) && not (head st == '^') =
        [head st] : convert inp (tail st)
      | (head inp `isSubsequenceOf` "*/+-^") && not (checkPrec (head inp) [(head st)]) =
        [head st] : [head (tail st)] : convert inp (tail (tail st))

getPrec xs a = snd (head (filter ((== a) . fst) xs))

checkPrec :: String -> String -> Bool
checkPrec x y = getPrec precedence x > getPrec precedence y

eval :: [String] -> [Double] -> Double
eval inp st
  | inp == [] = head st
  | all (== True) (map isNumber (head inp)) =
    eval (tail inp) (st ++ [read (head inp) :: Double])
  | head inp == "+" =
    eval (tail inp) (init (init st) ++ [last (init st) + last st])
  | head inp == "-" =
    eval (tail inp) (init (init st) ++ [last (init st) - last st])
  | head inp == "*" =
    eval (tail inp) (init (init st) ++ [last (init st) * last st])
  | head inp == "/" =
    eval (tail inp) (init (init st) ++ [last (init st) / last st])
