module Uniforms exposing (..)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, mul, makeRotate, makePerspective, makeLookAt, transform)

import BlockModel


--import WebGL exposing (..)
perspective = makePerspective 45 1 0.01 100
camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)

--cameraWorkaround angleX = makeLookAt (transform (makeRotate angleX (vec3 1 0 0))(vec3 0 0 5)) (vec3 0 0 0) (transform (makeRotate angleX (vec3 1 0 0))(vec3 0 1 0))


--cameraWorkaround2 angleX angleY = makeLookAt (transform (makeRotate angleX (vec3 1 0 0))(vec3 0 0 5)) (vec3 0 0 0) (transform (makeRotate angleX (vec3 1 0 0))(vec3 0 1 0))
{-
uniforms : Float -> Float -> { rotation : Mat4, perspective : Mat4, camera : Mat4, shade : Float }
uniforms angleX angleY =
    { rotation = multiplyRotationXY angleX angleY
    , perspective = perspective
    , camera = camera
    , shade = 0.8
    }
-}
uniforms : Mat4 -> { rotation : Mat4, perspective : Mat4, camera : Mat4, shade : Float }
uniforms matrix =
    { rotation = matrix
    , perspective = perspective
    , camera = camera
    , shade = 1--0.8
    }
{-
uniformsRow : Float -> Float -> Float -> BlockModel.Axis -> BlockModel.Turn -> { rotation : Mat4, perspective : Mat4, camera : Mat4, shade : Float }
uniformsRow angleX angleY spin axis turn =
    { rotation = mul (multiplyRotationXY angleX angleY) (makeRotate spin (calculateRowRotationVec axis turn))
    , perspective = perspective
    , camera = camera
    , shade = 0.8
    }
-}
uniformsRow : Mat4 -> Float -> BlockModel.Axis -> BlockModel.Turn -> { rotation : Mat4, perspective : Mat4, camera : Mat4, shade : Float }
uniformsRow matrix spin axis turn =
    { rotation = mul (matrix) (makeRotate spin (calculateRowRotationVec axis turn))
    , perspective = perspective
    , camera = camera
    , shade = 1--0.8
    }
{-
multiplyRotationXY angleX angleY =
  --mul (makeRotate angleY (vec3 0 1 0)) (makeRotate angleX (vec3 1 0 0))
  mul (makeRotate angleX (vec3 1 0 0)) (makeRotate angleY (vec3 0 1 0))
  --makeRotate 1 (vec3 angleX angleY 0)
  --makeRotate angleY (vec3 0 1 0)
  --mul (makeRotate angleX (vec3 -1 0 0)) (makeRotate angleY (calculateRotatedYAxis angleX angleY))
  --mul (makeRotate angleX (calculateRotatedXAxis angleX angleY)) (makeRotate angleY (vec3 0 1 0))
  --mul (makeRotate angleY (calculateRotatedXAxis angleX angleY)) (makeRotate angleY (calculateRotatedYAxis angleX angleY))

calculateRotatedYAxis angleX angleY =
  --transform (makeRotate angleX (vec3 1 0 0)) (transform (makeRotate angleY (vec3 0 1 0)) (vec3 0 1 0))
  transform (makeRotate angleX (vec3 1 0 0)) (vec3 0 1 0)
  --transform (mul(makeRotate angleY (vec3 0 1 0))(makeRotate angleX (vec3 1 0 0))) (vec3 0 1 0)

calculateRotatedXAxis angleX angleY =
  transform (makeRotate angleX (vec3 0 1 0)) (vec3 1 0 0)
  --transform (makeRotate angleX (vec3 1 0 0)) (transform (makeRotate angleY (vec3 0 1 0)) (vec3 1 0 0))
  --transform (mul(makeRotate angleY (vec3 0 1 0))(makeRotate angleX (vec3 1 0 0))) (vec3 1 0 0)
-}
calculateRowRotationVec: BlockModel.Axis -> BlockModel.Turn -> Vec3
calculateRowRotationVec axis turn =
  let
    direction = if turn == BlockModel.Clock then 1 else -1
  in
    case axis of
      BlockModel.X ->
        vec3 direction 0 0
      BlockModel.Y ->
        vec3 0 direction 0
      BlockModel.Z ->
        vec3 0 0 direction
