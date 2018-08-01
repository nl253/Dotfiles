module Main where

import Data.Foldable (concatMap)
import System.Environment (getArgs)

enumerate :: (Integral n, Show n) => [a] -> [(n, a)]
enumerate xs = go 0 xs
  where
    go :: (Integral n, Show n) => n -> [a] -> [(n, a)]
    go _ [] = []
    go n (x:xs) = (n, x) : (go (succ n) xs)

enumLinesInFile :: String -> IO String
enumLinesInFile fileName = do
  fileContent <- readFile fileName
  pure $ concatMap showPair $ enumerate $ lines fileContent
  where
    showPair :: (Show n) => (n, String) -> String
    showPair (n, line) = show n <> " " <> line <> "\n"

main :: IO ()
main = do
  fileName <- getArgs >>= (pure . head)
  s <- enumLinesInFile fileName
  putStrLn $ "reading " <> (show fileName) <> "\n\n" <> s
