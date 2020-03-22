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

findDepth :: String -> DepthMap -> Maybe Int
findDepth name depthMap = case filter (\(n, _) -> n == name) depthMap of
  [(_, d)] -> Just d
  _        -> error $ "Not found: " ++ name ++ show depthMap

fromParseTree :: NamedTerm String -> DepthMap -> Maybe Term
fromParseTree (NVar a) depthMap              = Var <$> findDepth a depthMap
fromParseTree (NApply l r) depthMap          = Apply <$> fromParseTree l depthMap <*> fromParseTree r depthMap
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
    Nothing -> putStrLn "Couldn't parse :/"
    Just r  -> print r
  putStrLn $ "Parse tree     : " ++ show (parseOrError (showTerm 0 term))
