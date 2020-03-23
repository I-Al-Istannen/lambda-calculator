module Lambda.Interactive where

import           Data.Char
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
  | c == '\n' = return readText
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

runInterpreter :: IO ()
runInterpreter = do
  putStr "Input: "
  input <- readLine
  if null input then
    putStrLn "\nGoodbye!"
  else do
    case parseInput input of
      Left err -> putStrLn $ errorBundlePretty err
      Right tree -> case fromParseTree tree [] of
        Nothing   -> putStrLn "Could not transform the parse tree :/"
        Just term -> printSteps term
    runInterpreter
