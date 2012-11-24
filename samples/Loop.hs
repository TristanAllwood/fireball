module Loop where

main :: IO ()
main = print . sum . map foo $ [1, 2, 3, 4, 5]

foo 0 = 0
foo n = foo (n - 1)
