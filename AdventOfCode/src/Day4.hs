module Day4(charPat, doProblem) where
  import Text.Regex.TDFA
  import Text.Regex.TDFA.String

  charPat :: Regex
  charPat = makeRegex "[a-z]+"
  numPat :: Regex
  numPat = makeRegex "[0-9]+"

  test:: String
  test = "fubrjhqlf-edvnhw-dftxlvlwlrq-803[wjvzd]"

  doProblem :: [String]
  doProblem = match charPat test
