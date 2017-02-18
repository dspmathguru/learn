module Day2(doMove, doLine, loc2Num, doPuzzle, answerFromFile) where

  doMove :: (Int, Int) -> Char -> (Int, Int)
  doMove (x, y) 'D' = (x, if y + 1 > 2 then 2 else y + 1)
  doMove (x, y) 'U' = (x, if y - 1 < 0 then 0 else y - 1)
  doMove (x, y) 'L' = (if x - 1 < 0 then 0 else x - 1, y)
  doMove (x, y) 'R' = (if x + 1 > 2 then 2 else x + 1, y)
  doMove (x, y) _ = (x, y)

  doLine :: (Int, Int) -> String -> (Int, Int)
  doLine = foldl doMove

  loc2Num :: (Int, Int) -> Int
  loc2Num (0, 0) = 1
  loc2Num (1, 0) = 2
  loc2Num (2, 0) = 3
  loc2Num (0, 1) = 4
  loc2Num (1, 1) = 5
  loc2Num (2, 1) = 6
  loc2Num (0, 2) = 7
  loc2Num (1, 2) = 8
  loc2Num (2, 2) = 9
  loc2Num _      = -1

  doPuzzle :: [String] -> [Int]
  doPuzzle ss = map loc2Num (puzzle (1, 1) ss)
    where
      puzzle :: (Int, Int) -> [String] -> [(Int, Int)]
      puzzle _ [] = []
      puzzle i (x:xs) = l : puzzle l xs
        where
            l = doLine i x

  answerFromFile :: FilePath -> IO()
  answerFromFile fileName =
    do
      contents <- readFile fileName
      putStr (unlines (map show (doPuzzle $ lines contents)))
