module CalculateViewFromDrag exposing(..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, mul, makeRotate, makePerspective, makeLookAt, transform)


calculateView: Mat4 -> Float -> Float -> Mat4
calculateView matrix x y =
  let
    dragMatrix  = mul (makeRotate x (vec3 -1 0 0)) (makeRotate y (vec3 0 -1 0))
  in
    mul dragMatrix matrix

{-
calculateView: Mat4 -> Float -> Float -> Mat4
calculateView matrix x y =
  let
    dragMatrix  = makeRotate x (vec3 1 1 0)
  in
    mul dragMatrix matrix
    -}



