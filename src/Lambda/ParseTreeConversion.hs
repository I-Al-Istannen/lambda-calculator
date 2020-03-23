module Lambda.ParseTreeConversion where

import           Lambda.MegaParsing
import           Lambda.Term

type DepthMap = [(String, Int)]

mapSecond :: (b -> c) -> (a, b) -> (a, c)
mapSecond f (a, b) = (a, f b)

addLambda :: String -> DepthMap -> DepthMap
addLambda name depthMap = (name, 0) : map (mapSecond (+1)) others
  where
    others = removeLambda name depthMap

removeLambda :: String ->DepthMap -> DepthMap
removeLambda name = filter (\(n, _) -> n /= name)

findDepth :: String -> DepthMap -> Either String Int
findDepth name depthMap = case filter (\(n, _) -> n == name) depthMap of
  [(_, d)] -> Right d
  _        -> Left $ "Variable '" ++ name ++ "' not found!"

fromParseTree :: NamedTerm String -> DepthMap -> Either String Term
fromParseTree (NVar a) depthMap              = Var <$> findDepth a depthMap
fromParseTree (NApply l r) depthMap          = Apply <$> fromParseTree l depthMap <*> fromParseTree r depthMap
fromParseTree (KnownTerm t) _                = pure t
fromParseTree (NLambda varName val) depthMap = let
  newMap = addLambda varName depthMap
  in Lambda <$> fromParseTree val newMap

showAndPrintParsed :: Term -> IO ()
showAndPrintParsed term = do
  putStr "Pretty printed : "
  putStrLn $ showTerm 0 term
  putStr "It should be   : "
  print term
  putStr "Parsed was     : "
  case fromParseTree (parseOrError (showTerm 0 term)) [] of
    Left msg -> putStrLn $ "Couldn't parse: " ++ msg
    Right r  -> print r
  putStrLn $ "Parse tree     : " ++ show (parseOrError (showTerm 0 term))
