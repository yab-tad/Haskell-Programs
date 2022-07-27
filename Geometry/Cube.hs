module Geometry.Cube 
(area, volume) where

import qualified Geometry.Cuboid as Cuboid

area :: Float -> Float
area side = Cuboid.area side side side

volume :: Float -> Float
volume side = Cuboid.volume side side side
