module Arithmetic exposing(..)

isEven: Int -> Bool
isEven x =
  x % 2 == 0

modFloat : Float -> Float -> Float
modFloat x m = x - m * toFloat (floor (x/m))
