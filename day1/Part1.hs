module Part1 where

main :: IO()
main = do
  fileTxt <- readFile "input.txt"
  let measurements = read <$> lines fileTxt
  let result = biggerThanLast measurements
  print result

biggerThanLast :: [Int] -> Int
biggerThanLast list = sum [if y > x then 1 else 0 | (x,y) <- zip list (tail list)]
