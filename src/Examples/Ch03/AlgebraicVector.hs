module Ch03.AlgebraicVector
    (
        Cartesian2D(..),
        Polar2D(..),
    )
    where

{-- snippet types --}
-- x and y coordinates or lengths.
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

-- Angle and distance (magnitude).
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)
{-- /snippet types --}
