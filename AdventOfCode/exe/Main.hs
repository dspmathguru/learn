module Main where

import Day1
import Text.ParserCombinators.Parsec

main :: IO ()
main =
    do c <- getContents
       case parseCSV c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> do
                          print l
                          print $ distance l
              where
                  p = makePuzzle $ concat r
                  l = doPuzzle startLoc p
