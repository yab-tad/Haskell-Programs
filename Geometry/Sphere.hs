module Geometry.Sphere
(area, volume) where

area :: Float -> Float
area radius = 4 * pi * (radius ^2)

volume :: Float -> Float
volume radius = (4.0 / 3.0) * pi * (radius ^3)