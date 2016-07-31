module Shapes
( Shape(..)
, surface
, nudge
, baseCircle
, baseRectangle
) where

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r^2
surface (Rectangle _ _ w h) = w * h

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle x y r) a b      = Circle (x + a) (y + b) r
nudge (Rectangle x y w h) a b = Rectangle (x + a) (y + b) w h

baseCircle :: Float -> Shape
baseCircle r = Circle 0 0 r

baseRectangle :: Float -> Float -> Shape
baseRectangle w h = Rectangle 0 0 w h
