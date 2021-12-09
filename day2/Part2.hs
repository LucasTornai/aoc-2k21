module Part2 where

main :: IO()
main = do
  fileTxt <- readFile "input.txt"
  let commands = lines fileTxt
  let parsedCommands = parseCommand <$> commands
  let commandTuples = commandToTuple <$> parsedCommands
  let (x, y, z) = foldl finalPosition (0, 0, 0) commandTuples
  print (x * z)

parseCommand :: String -> (String, Int)
parseCommand command = (direction, value) where
  splitCommand = words command
  direction = head splitCommand
  value = read $ last splitCommand

commandToTuple :: (String, Int) -> (Int, Int, Int)
commandToTuple ("forward", x) = (x, 0, 0)
commandToTuple ("down", x)    = (0, x, 0)
commandToTuple ("up", x)      = (0, -x, 0)
commandToTuple _              = (0, 0, 0)

finalPosition :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
finalPosition (x1, x2, x3) (0, y2, y3)  = (x1, x2 + y2, x3)
finalPosition (x1, x2, x3) (y1, y2, y3) = (x1 + y1, x2 + y2, x3 + (x2 * y1))
