import Data.Strings
import Data.List
import System.REPL

palindrome: Nat -> String -> Bool
palindrome n x = let y = toLower x in length y >= n && (reverse y) == y

counts: String -> (Nat, Nat)
counts x = (length (words x), length x)

top_ten: Ord a => List a -> List a
top_ten l = take 10 (reverse (sort l))

over_length: Nat -> List String -> Nat
over_length n l = length (filter (> n) (map length l))

main_palindrome: IO ()
main_palindrome = repl "Enter a string: " (\x => show (palindrome 0 x))

main_counts: IO ()
main_counts = repl "Enter a string: " (\x => show (counts x))

main: IO ()
main = main_counts
