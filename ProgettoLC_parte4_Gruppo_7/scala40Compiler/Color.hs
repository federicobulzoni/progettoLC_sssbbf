module Color where

-- Ref: https://en.wikipedia.org/wiki/ANSI_escape_code

data Color
    = Black  
    | Red    
    | Green  
    | Yellow 
    | Blue   
    | Magenta
    | Cyan   
    | White
    | Default 

data Style 
    = Normal
    | Bold
    | Faint
    | Italic
    | Underline
    | Fraktur


endingColor :: String
endingColor = "\x1b[0m"

getColor :: Color -> String
getColor c = case c of
    Black   -> "30" 
    Red     -> "31"
    Green   -> "32"
    Yellow  -> "33"
    Blue    -> "34"
    Magenta -> "35"
    Cyan    -> "36"
    White   -> "37"
    Default -> ""

getStyle :: Style -> String
getStyle s = case s of
    Normal      -> "0"
    Bold        -> "1"
    Faint       -> "2"
    Italic      -> "3"
    Underline   -> "4"
    Fraktur     -> "20"

buildColor :: Color -> Style -> String
buildColor c s = "\x1b[" ++ getStyle s ++ ";" ++ getColor c ++ "m" 

color :: Color -> Style -> String -> String
color c s word = (buildColor c s) ++ word ++ endingColor
