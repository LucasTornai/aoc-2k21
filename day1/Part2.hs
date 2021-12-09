module Part2 where

main :: IO()
main = do
  fileTxt <- readFile "input.txt"
  let measurements = read <$> lines fileTxt
  let windowMeasurements = makeWindow measurements
  let result = biggerThanLast windowMeasurements
  print result

biggerThanLast :: [Int] -> Int
biggerThanLast list = sum [if y > x then 1 else 0 | (x,y) <- zip list (tail list)]

makeWindow :: [Int] -> [Int]
makeWindow [] = []
makeWindow list@(x:xs)
  | length list < 3 = []
  | otherwise       = (sum $ take 3 list) : (makeWindow xs)
