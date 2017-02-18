module Day1
  (parseCSV, csvFile, makePuzzle, doPuzzle, Go(..),
   Dir(..), Location(..), distance, move, startLoc) where

  import Text.ParserCombinators.Parsec

  csvFile = endBy line eol
  line = sepBy cell (char ',')
  cell = quotedCell <|> (skipMany space >> many (noneOf ",\n\r"))

  quotedCell =
      do char '"'
         content <- many quotedChar
         char '"' <?> "quote at end of cell"
         return content

  quotedChar =
          noneOf "\""
      <|> try (string "\"\"" >> return '"')

  eol =   try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

  parseCSV :: String -> Either ParseError [[String]]
  parseCSV = parse csvFile "(unknown)"

  data Go = R Int | L Int
  instance Show Go where
    show (R i) = 'R' : show i
    show (L i) = 'L' : show i

  data Dir = North | East | South | West
  instance Show Dir where
    show North = "N"
    show South = "S"
    show East  = "E"
    show West  = "W"

  data Location = Loc Dir Int Int
  instance Show Location where
    show (Loc d i j) = "Loc: " ++ show d ++ ": " ++ show i ++ ", " ++ show j

  toGo :: String -> Go
  toGo ('R' : ss) = R (read ss)
  toGo ('L' : ss) = L (read ss)
  toGo _       = R 0

  move :: Go -> Location -> Location
  move (R i) (Loc North x y) = Loc East (x + i) y
  move (L i) (Loc North x y) = Loc West (x - i) y
  move (R i) (Loc East x y)  = Loc South x (y - i)
  move (L i) (Loc East x y)  = Loc North x (y + i)
  move (R i) (Loc South x y) = Loc West (x - i) y
  move (L i) (Loc South x y) = Loc East (x + i) y
  move (R i) (Loc West x y)  = Loc North x (y + i)
  move (L i) (Loc West x y)  = Loc South x (y - i)

  makePuzzle :: [String] -> [Go]
  makePuzzle = map toGo

  doPuzzle :: Location -> [Go] -> Location
  doPuzzle l g = foldr move l $ reverse g

  distance :: Location -> Int
  distance (Loc _ x y) = abs x + abs y

  startLoc :: Location
  startLoc = Loc North 0 0
