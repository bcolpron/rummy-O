module Tiles where

data Color = Red | Blue | Black | Yellow deriving(Eq, Ord, Show)
data Tile = Tile Int Color | Joker deriving(Eq, Ord, Show)

number :: Tile -> Int
number (Tile val _) = val

color :: Tile -> Color
color (Tile _ c ) = c

black1  = Tile 1  Black
black2  = Tile 2  Black
black3  = Tile 3  Black
black4  = Tile 4  Black
black5  = Tile 5  Black
black6  = Tile 6  Black
black7  = Tile 7  Black
black8  = Tile 8  Black
black9  = Tile 9  Black
black10 = Tile 10 Black
black11 = Tile 11 Black
black12 = Tile 12 Black
black13 = Tile 13 Black
blue1  = Tile 1  Blue
blue2  = Tile 2  Blue
blue3  = Tile 3  Blue
blue4  = Tile 4  Blue
blue5  = Tile 5  Blue
blue6  = Tile 6  Blue
blue7  = Tile 7  Blue
blue8  = Tile 8  Blue
blue9  = Tile 9  Blue
blue10 = Tile 10 Blue
blue11 = Tile 11 Blue
blue12 = Tile 12 Blue
blue13 = Tile 13 Blue
red1  = Tile 1  Red
red2  = Tile 2  Red
red3  = Tile 3  Red
red4  = Tile 4  Red
red5  = Tile 5  Red
red6  = Tile 6  Red
red7  = Tile 7  Red
red8  = Tile 8  Red
red9  = Tile 9  Red
red10 = Tile 10 Red
red11 = Tile 11 Red
red12 = Tile 12 Red
red13 = Tile 13 Red
yellow1  = Tile 1  Yellow
yellow2  = Tile 2  Yellow
yellow3  = Tile 3  Yellow
yellow4  = Tile 4  Yellow
yellow5  = Tile 5  Yellow
yellow6  = Tile 6  Yellow
yellow7  = Tile 7  Yellow
yellow8  = Tile 8  Yellow
yellow9  = Tile 9  Yellow
yellow10 = Tile 10 Yellow
yellow11 = Tile 11 Yellow
yellow12 = Tile 12 Yellow
yellow13 = Tile 13 Yellow
joker = Joker
