module RotationDirectionFromDrag exposing(..)

import Math.Vector3 exposing (Vec3, vec3, getX, getY)
import Math.Matrix4 exposing (Mat4, mul, makeRotate, makePerspective, makeLookAt, transform)
import BlockModel exposing (Axis(..), Turn(..), Row, Move, Face)

a = 1

type TwoDDirection = Left | Right | Up | Down

getRadiansAbsoluteDifference: Float -> Float -> Float
getRadiansAbsoluteDifference a b =
  let
    diff = abs(a - b)
  in
    case diff > pi of
      False ->
        diff
      True ->
        pi - (diff-pi)

getClosestDirection: Float -> Float -> Float -> Float -> Float -> TwoDDirection
getClosestDirection left right up down direction =
  let
    deltaLeft = getRadiansAbsoluteDifference direction left --abs (direction - left)
    deltaRight = getRadiansAbsoluteDifference direction right --abs(direction - right)
    deltaUp = getRadiansAbsoluteDifference direction up --abs(direction - up)
    deltaDown = getRadiansAbsoluteDifference direction down --abs(direction - down)
  in
    if deltaLeft < deltaRight && deltaLeft < deltaUp && deltaLeft < deltaDown then
      Left
    else if deltaRight < deltaUp && deltaRight < deltaDown then
      Right
    else if deltaUp < deltaDown then
      Up
    else
      Down

calculateMove: Mat4 -> Float -> Float -> Face -> Move
calculateMove rotationMatrix x y face =
  let
    vecX = transform rotationMatrix (vec3 1 0 0)
    vecY = transform rotationMatrix (vec3 0 1 0)
    vecZ = transform rotationMatrix (vec3 0 0 1)
    dirX = atan2 (getX vecX) (getY vecX)
    dirY = atan2 (getX vecY) (getY vecY)
    dirZ = atan2 (getX vecZ) (getY vecZ)
    dirMX = atan2 -(getX vecX) -(getY vecX) --m for minus
    dirMY = atan2 -(getX vecY) -(getY vecY)
    dirMZ = atan2 -(getX vecZ) -(getY vecZ)
    dirD = atan2 x y
  in
    case face.direction of
      BlockModel.Top ->
        let
          dir = getClosestDirection dirX dirMX dirZ dirMZ dirD
        in
          case dir of
            Left ->
              { row = face.rows.x
              , turn = Anti
              , axis = X }
            Right ->
              { row = face.rows.x
              , turn = Clock
              , axis = X }
            Up ->
              { row = face.rows.z
              , turn = Anti
              , axis = Z }
            Down ->
              { row = face.rows.z
              , turn = Clock
              , axis = Z }
      BlockModel.Down ->
        let
          dir = getClosestDirection dirX dirMX dirZ dirMZ dirD
        in
          case dir of
            Left ->
              { row = face.rows.x
              , turn = Anti
              , axis = X }
            Right ->
              { row = face.rows.x
              , turn = Clock
              , axis = X }
            Up ->
              { row = face.rows.z
              , turn = Anti
              , axis = Z }
            Down ->
              { row = face.rows.z
              , turn = Clock
              , axis = Z }
      BlockModel.Left ->
        let
          dir = getClosestDirection dirY dirMY dirZ dirMZ dirD
        in
          case dir of
            Left ->
              { row = face.rows.y
              , turn = Anti
              , axis = Y }
            Right ->
              { row = face.rows.y
              , turn = Clock
              , axis = Y }
            Up ->
              { row = face.rows.z
              , turn = Anti
              , axis = Z }
            Down ->
              { row = face.rows.z
              , turn = Clock
              , axis = Z }
      BlockModel.Right ->
        let
          dir = getClosestDirection dirY dirMY dirZ dirMZ dirD
        in
          case dir of
            Left ->
              { row = face.rows.y
              , turn = Anti
              , axis = Y }
            Right ->
              { row = face.rows.y
              , turn = Clock
              , axis = Y }
            Up ->
              { row = face.rows.z
              , turn = Anti
              , axis = Z }
            Down ->
              { row = face.rows.z
              , turn = Clock
              , axis = Z }
      BlockModel.Front ->
        let
          dir = getClosestDirection dirX dirMX dirY dirMY dirD
        in
          case dir of
            Left ->
              { row = face.rows.x
              , turn = Anti
              , axis = X }
            Right ->
              { row = face.rows.x
              , turn = Clock
              , axis = X }
            Up ->
              { row = face.rows.y
              , turn = Anti
              , axis = Y }
            Down ->
              { row = face.rows.y
              , turn = Clock
              , axis = Y }
      BlockModel.Back ->
        let
          dir = getClosestDirection dirX dirMX dirY dirMY dirD
        in
          case dir of
            Left ->
              { row = face.rows.x
              , turn = Anti
              , axis = X }
            Right ->
              { row = face.rows.x
              , turn = Clock
              , axis = X }
            Up ->
              { row = face.rows.y
              , turn = Anti
              , axis = Y }
            Down ->
              { row = face.rows.y
              , turn = Clock
              , axis = Y }



