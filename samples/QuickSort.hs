module QuickSort where

main :: IO ()
main = print . notQuickSort $
  [ 22, 80, 6, 16, 9, 31, 38, 50, 67, 57, 44, 31, 63, 37, 8, 27, 24, 82, 95, 9
  , 30, 45, 17, 100, 97, 44, 26, 14, 68, 2, 3, 44, 30, 7, 56, 73, 40, 89, 86, 7, 5
  , 83, 26, 14, 13, 33, 41, 35, 97, 28, 75, 67, 73, 23, 69, 67, 83, 60, 80, 48
  , 100, 35, 61, 15, 62, 69, 35, 61, 38, 94, 88, 42, 96, 75, 81, 4, 68, 39, 52, 79
  , 49, 88, 96, 50, 86, 76, 47, 99, 87, 25, 76, 10, 24, 80, 91, 76, 68, 86, 3, 30 ]

notQuickSort :: Ord a => [a] -> [a]
notQuickSort [] = []
notQuickSort (p:xs) = (notQuickSort lesser) ++ [p] ++ (notQuickSort greater)
  where
    lesser = filter (<p) xs
    greater = filter (>=p) xs
