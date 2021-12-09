module Part1 where

main :: IO()
main = do
  fileTxt <- readFile "input.txt"
  let commands = lines fileTxt
  let parsedCommands = parseCommand <$> commands
  let commandTuples = commandToTuple <$> parsedCommands
  let (x, y) = foldl addTuples (0, 0) commandTuples
  print (x * y)

parseCommand :: String -> (String, Int)
parseCommand command = (direction, value) where
  splitCommand = words command
  direction = head splitCommand
  value = read $ last splitCommand

commandToTuple :: (String, Int) -> (Int, Int)
commandToTuple ("forward", x) = (x, 0)
commandToTuple ("down", x)    = (0, x)
commandToTuple ("up", x)      = (0, -x)
commandToTuple _              = (0, 0)

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)
