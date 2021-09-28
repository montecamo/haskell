import System.Random
import Text.Read

data Guess = Rock | Paper | Scissors deriving (Eq, Read, Show)

newtype UserGuess = UserGuess {runUserGuess :: Maybe Guess}

newtype ComputerGuess = ComputerGuess {runComputerGuess :: Guess}

data GameResult = UserWon | ComputerWon | Draw

instance Show GameResult where
  show UserWon = "You won"
  show ComputerWon = "Computer won"
  show Draw = "Draw, haha"

getComputerGuess :: IO ComputerGuess
getComputerGuess = ComputerGuess . randomGuess <$> newStdGen

randomGuess :: StdGen -> Guess
randomGuess g =
  let (r, _) = (randomR (0, 2) g :: (Int, StdGen))
   in case r of
        0 -> Rock
        1 -> Paper
        _ -> Scissors

askUserGuess :: IO ()
askUserGuess = putStrLn "Input your guess:"

readUserGuess :: IO UserGuess
readUserGuess = do
  guess <- getLine
  return $ UserGuess (readMaybe guess :: Maybe Guess)

getUserGuess :: IO UserGuess
getUserGuess = askUserGuess >> readUserGuess

getGameResult :: UserGuess -> ComputerGuess -> GameResult
getGameResult (UserGuess Nothing) _ = ComputerWon
getGameResult (UserGuess (Just Rock)) (ComputerGuess Rock) = Draw
getGameResult (UserGuess (Just Rock)) (ComputerGuess Paper) = ComputerWon
getGameResult (UserGuess (Just Rock)) (ComputerGuess Scissors) = UserWon
getGameResult (UserGuess (Just Paper)) (ComputerGuess Rock) = UserWon
getGameResult (UserGuess (Just Paper)) (ComputerGuess Paper) = Draw
getGameResult (UserGuess (Just Paper)) (ComputerGuess Scissors) = ComputerWon
getGameResult (UserGuess (Just Scissors)) (ComputerGuess Rock) = ComputerWon
getGameResult (UserGuess (Just Scissors)) (ComputerGuess Paper) = UserWon
getGameResult (UserGuess (Just Scissors)) (ComputerGuess Scissors) = Draw

play :: IO ()
play = do
  pguess <- getUserGuess
  cguess <- getComputerGuess
  print $ getGameResult pguess cguess
