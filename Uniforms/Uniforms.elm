module Uniforms exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, mul, makeRotate, makePerspective, makeLookAt)

import BlockModel


--import WebGL exposing (..)


uniforms : Float -> { rotation : Mat4, perspective : Mat4, camera : Mat4, shade : Float }
uniforms t =
    { rotation = mul (makeRotate (3 * t) (vec3 0 1 0)) (makeRotate (2 * t) (vec3 1 0 0))
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    }


uniformsRow : Float -> Float -> BlockModel.Axis -> BlockModel.Turn -> { rotation : Mat4, perspective : Mat4, camera : Mat4, shade : Float }
uniformsRow t spin axis turn =
    { rotation = mul (mul (makeRotate (3 * t) (vec3 0 1 0)) (makeRotate (2 * t) (vec3 1 0 0))) (makeRotate spin (calculateRowRotationVec axis turn))
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    }

calculateRowRotationVec: BlockModel.Axis -> BlockModel.Turn -> Vec3
calculateRowRotationVec axis turn =
  let
    direction = if turn == BlockModel.Right then 1 else -1
  in
    case axis of
      BlockModel.X ->
        vec3 direction 0 0
      BlockModel.Y ->
        vec3 0 direction 0
      BlockModel.Z ->
        vec3 0 0 direction
