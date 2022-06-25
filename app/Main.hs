import System.IO.Unsafe
import System.Random
import Data.Char
import qualified Data.Text
import System.Console.ANSI


main :: IO ()


-- Introduction to the game
main = do
    clearScreen
    setCursorPosition 0 0
    setSGR [SetColor Foreground Dull Yellow]
    putStrLn "Lets play Rock Paper Scissors!"
    putStrLn "    _______"
    putStrLn "---'   ____)"
    putStrLn "      (_____)"
    putStrLn "      (_____)"
    putStrLn "      (____)"
    putStrLn "---.__(___)"
    play


moves :: [String]
moves = [ "ROCK", "PAPER", "SCISSORS"]


-- Computer Randomly Generates a number which converts into either ROCK,PAPER or SCISSORS from the list of strings,  moves. 
aiMove :: String
aiMove = moves!!unsafePerformIO (getStdRandom (randomR (0, 2)))

--Compare the users input to the computers input.
eval :: String -> String -> String
eval u c
    | u == c                                      = draw
    | u == "ROCK"       &&     c == "PAPER"       = aiWin
    | u == "ROCK"       &&     c == "SCISSORS"    = uWin
    | u == "PAPER"      &&     c == "ROCK"        = uWin
    | u == "PAPER"      &&     c == "SCISSORS"    = aiWin
    | u == "SCISSORS"   &&     c == "ROCK"        = aiWin
    | u == "SCISSORS"   &&     c == "PAPER"       = uWin
    | otherwise                                   = err
    where
        draw = "Draw! You both chose " ++ u ++ "."
        uWin = "WINNER! You chose " ++ u ++ " while the AI chose " ++ c ++ "." 
        cWin = "Game Over, You lost! You chose " ++ u ++ " while the AI chose " ++ c ++ "."
        err  = "Error: that move doesn't exist. Please pick either Rock, Paper, or Scissors."


--Takes user input for choice
play:: IO()
play = do
    putStrLn "Do you choose Rock, Paper, or Scissors?"
    userInp <- getLine 
    putStrLn "Rock, Paper, Scissors!"
    putStrLn (eval (map toUpper userInp) aiMove)
    continue
       
continue :: IO()

continue = do 
    putStrLn "Do you want to play again? (Y/N): "
    playAgain <- getLine
    if playAgain == "y" || playAgain == "Y" 
        then do play
    else if playAgain == "n" || playAgain == "N"
        then putStrLn "Thanks for playing!"
    else putStrLn "Error: Not valid option. Game will exit. Thanks for playing!"

