module Pictures
  ( Picture
  , horse
  , flipV
  , flipH
  , above
  , beside
  , superimpose
  , invertColour
  )
  where

newtype Picture = Picture [[Char]]

-- The example used in Craft2e: a polygon which looks like a horse. Here
-- taken to be a 16 by 12 rectangle.

horse :: Picture

horse = Picture
        ["       ##   ",
         "     ##  #  ",
         "   ##     # ",
         "  #       # ",
         "  #   #   # ",
         "  #   ### # ",
         " #    #  ## ",
         "  #   #     ",
         "   #   #    ",
         "    #  #    ",
         "     # #    ",
         "      ##    "]

-- Completely white and black pictures.

white :: Picture

white = Picture
        ["      ",
         "      ",
         "      ",
         "      ",
         "      ",
         "      "]

black = Picture
        ["######",
         "######",
         "######",
         "######",
         "######",
         "######"]

-- Getting a picture onto the screen.

instance Show Picture where
  show (Picture p) = concat . map (++"\n") $ p

-- Transformations of pictures.

pic :: ([[Char]] -> [[Char]]) -> Picture -> Picture
pic f (Picture p) = Picture $ f p

pic2 :: ([[Char]] -> [[Char]] -> [[Char]]) -> Picture -> Picture -> Picture
pic2 f (Picture p1) (Picture p2) = Picture $ f p1 p2

-- Reflection in a vertical mirror.

flipV :: Picture -> Picture

flipV = pic $ map reverse

-- Reflection in a horizontal mirror.

flipH :: Picture -> Picture

flipH = pic reverse

-- Rotation through 180 degrees, by composing vertical and horizontal
-- reflection. Note that it can also be done by flipV.flipH, and that we
-- can prove equality of the two functions.

rotate :: Picture -> Picture

rotate = flipH . flipV

-- One picture above another. To maintain the rectangular property,
-- the pictures need to have the same width.

above :: Picture -> Picture -> Picture

above = pic2 (++)

-- One picture next to another. To maintain the rectangular property,
-- the pictures need to have the same height.

beside :: Picture -> Picture -> Picture

beside = pic2 $ zipWith (++)

-- Superimose one picture above another. Assume the pictures to be the same
-- size. The individual characters are combined using the combine function.

superimpose :: Picture -> Picture -> Picture

superimpose = pic2 $ zipWith (zipWith combine)

-- For the result to be '.' both components have to the '.'; otherwise
-- get the '#' character.

combine :: Char -> Char -> Char

combine topCh bottomCh
  = if (topCh == ' ' && bottomCh == ' ')
    then ' '
    else '#'

-- Inverting the colours in a picture; done pointwise by invert...

invertColour :: Picture -> Picture

invertColour = pic $ map (map invert)

-- ... which works by making the result '.' unless the input is '.'.

invert :: Char -> Char

invert ch = if ch == ' ' then '#' else ' '
