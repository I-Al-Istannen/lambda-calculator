module Lambda.Interactive where

import           Data.Char
import           Data.Foldable
import           Lambda.MegaParsing
import           Lambda.ParseTreeConversion
import           Lambda.Term
import           System.IO
import           Text.Megaparsec

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  char <- getChar
  hSetEcho stdin True
  return char

handleChar :: String -> Char -> IO String
handleChar readText c
  | ord c == 127 = do
    putStr "\b \b"
    readNext (init readText)
  | c == '\EOT' = return ""
  | c == '\n' = do
    putChar '\n'
    return readText
  | otherwise = do
    putChar c
    readNext $ readText ++ [c]
  where
    readNext newText = do
      char <- getCh
      handleChar newText char

readLine :: IO String
readLine = do
  char <- getCh
  handleChar "" char

printSteps' :: Term -> IO ()
printSteps' = traverse_ (\t -> putStrLn ("> " ++ showTerm 0 t)) . steps

runInterpreter :: IO ()
runInterpreter = do
  putStr "$ "
  input <- readLine
  if null input then
    putStrLn "\nGoodbye!"
  else do
    case parseInput input of
      Left err -> putStrLn $ errorBundlePretty err
      Right tree -> case fromParseTree tree [] of
        Left msg   -> putStrLn $ "Could not transform the parse tree. " ++ msg
        Right term -> printSteps' term
    runInterpreter
