module MainAngle exposing(..)

import Basics
import Arithmetic

calculateAngle: Float -> Float -> Float
calculateAngle old delta =
  let
    new = old + (delta/ 5000)
  in
    Arithmetic.modFloat new (2*Basics.pi)

calculateMoveRowAngle: Float -> Float -> Float
calculateMoveRowAngle old delta =
    old + (delta/ 300)
