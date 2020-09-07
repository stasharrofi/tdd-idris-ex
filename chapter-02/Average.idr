module Main

import Data.Strings
import Data.List
import System.REPL

average: String -> Double
average s = let numWords = wordCount s
                totalLength = sum (allLengths (words s)) in
                cast totalLength / cast numWords
  where
    wordCount: String -> Nat
    wordCount s = length (words s)

    allLengths: List String -> List Nat
    allLengths l = map length l

showAverage: String -> String
showAverage s = "The average word length is: " ++
                show (average s) ++ "\n"

main: IO ()
main = repl "Enter a string: " showAverage
