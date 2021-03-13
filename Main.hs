import Data.List
import System.IO
import System.Console.ANSI

data State = State {lives   :: Int
                   ,guesses :: [Char]
                   ,word    :: String}
             deriving (Show)

guess :: State -> Char -> State
guess s guess
  | guess `elem` (guesses s) = s
  | otherwise = if guess `elem` w then
        s {guesses = guess:(guesses s)}
      else
        s {lives= (lives s) - 1}
    where w = word s

vizualize :: State -> IO ()
vizualize s = do
  let l = lives s
      g = guesses s
      w = word s
      v = map (\x -> if x `elem` g then x else '_') w
  putStrLn $ "Lives: " ++ show l
  putStrLn v
  putStrLn ""

victory :: IO ()
victory = do
  putStrLn "Victory!"

gameOver :: IO ()
gameOver = do
  putStrLn "Game over :("

play :: State -> IO ()
play s = do
  g <- head <$> getLine
  resetScreen
  let s' = guess s g
  putStrLn ""
  vizualize s'
  if length (nub $ word s') == length (guesses s') then
    victory else
    if (lives s' < 1) then
      gameOver else
      play s'

resetScreen = do
  clearScreen
  setCursorPosition 0 0

main = do
  putStrLn "Enter a word: "
  word <- getLine
  case word of
    c:_ -> do
      resetScreen
      play $ State 6 [] word
    _   -> do
      putStrLn "Please put in at least one letter"
      main
