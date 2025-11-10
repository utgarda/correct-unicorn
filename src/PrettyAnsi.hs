module PrettyAnsi where

ansiBlack :: [Char]
ansiBlack = "\x1b[30m"
ansiRed :: [Char]
ansiRed = "\x1b[31m"
ansiGreen :: [Char]
ansiGreen = "\x1b[32m"
ansiYellow :: [Char]
ansiYellow = "\x1b[33m"
ansiBlue :: [Char]
ansiBlue = "\x1b[34m"
ansiMagenta :: [Char]
ansiMagenta = "\x1b[35m"
ansiCyan :: [Char]
ansiCyan = "\x1b[36m"
ansiWhite :: [Char]
ansiWhite = "\x1b[37m"
ansiReset :: [Char]
ansiReset = "\x1b[0m"
ansiBold :: [Char]
ansiBold = "\x1b[1m"

paintWords :: [[Char]] -> [[Char]] -> [[Char]]
paintWords ws colors =
    [color ++ word | (color, word) <- zip (cycle colors) ws]

bold :: [Char] -> [Char]
bold string = ansiBold ++ string

colorNameToAnsi :: String -> Maybe String
colorNameToAnsi "black" = Just ansiBlack
colorNameToAnsi "red" = Just ansiRed
colorNameToAnsi "green" = Just ansiGreen
colorNameToAnsi "yellow" = Just ansiYellow
colorNameToAnsi "blue" = Just ansiBlue
colorNameToAnsi "magenta" = Just ansiMagenta
colorNameToAnsi "cyan" = Just ansiCyan
colorNameToAnsi "white" = Just ansiWhite
colorNameToAnsi _ = Nothing
