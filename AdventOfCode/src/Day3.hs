module Day3(testTriangle, doPuzzle, answerFromFile) where

  testTriangle :: (Int, Int, Int) -> Bool
  testTriangle (x, y, z) = x+y >z && x+z > y && y+z > x

  doPuzzle :: String -> [Bool]
  doPuzzle s = map testTriangle ts
    where
      ws :: [[String]]
      ws = map words $ lines s
      ts :: [(Int, Int, Int)]
      ts = map l2triple ws
      l2triple :: [String] -> (Int, Int, Int)
      l2triple x = (read (x!!0), read (x!!1), read (x!!2))

  answerFromFile :: FilePath -> IO()
  answerFromFile fileName =
    do
      contents <- readFile fileName
      putStrLn (show $ sum $ map fromEnum  $ doPuzzle contents)
