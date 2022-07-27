module Geometry.Cuboid
(area, volume) where

area :: Float -> Float -> Float -> Float
area a b c = rectangle a b * c

volume :: Float -> Float -> Float -> Float
volume a b c = rectangle a b * 2 + rectangle a c * 2 + rectangle b c * 2

rectangle :: Float -> Float -> Float
rectangle a b = a * b