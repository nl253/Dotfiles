module Main where

enumerate :: Word -> [a] -> [(Word, a)]
enumerate _ []     = [] 
enumerate n (x:xs) = (n, x):(enumerate (succ n) xs)

main :: IO ()
main = do
  putStr "reading"
  l
  putStrLn fileName
  let fileName    = "/home/norbert/.bashrc"
        enumedLines = (enumerate 0 (lines fileText))
      largestNo   = fst $ head $ reverse enumedLines
   in { 
        putStrLn $ unlines $ fmap (\(n, line) -> (show n) <> " " <> line) 
        fileText <- readFile fileName
