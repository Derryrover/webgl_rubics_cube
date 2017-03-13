module MainUpdateFrame exposing(..)

import CommandToMessage
import MainModel exposing (Model, init)
import MainAngle exposing(calculateAngle,calculateMoveRowAngle)
import SpinCubeGl exposing(sceneSpinRow, sceneRest)

import MainMessage exposing(Msg(..))

updateFrame model dt =
  let
    moveAngle = calculateMoveRowAngle model.movingRowAngle dt
    viewAngle = calculateAngle model.angle dt
  in
    case (moveAngle > (0.5*Basics.pi)) of
      True ->
        ({ model | angle = viewAngle
                 , movingRowAngle = 0}
        , CommandToMessage.message EndMove)
      False ->
        ({ model | angle = viewAngle
                , movingRowAngle = moveAngle}
        , Cmd.none)


{-
dragStart model position =
      ({ model | formerDragX = toFloat position.x
               , formerDragY = toFloat position.y
               , dragging = True} , Cmd.none)

  let
    moveAngle = calculateMoveRowAngle model.movingRowAngle dt
    viewAngle = calculateAngle model.angle dt
  in
    case (moveAngle > (0.5*Basics.pi)) of
      True ->
        ({ model | angle = viewAngle
                 , movingRowAngle = 0}
        , CommandToMessage.message EndMove)
      False ->
        ({ model | angle = viewAngle
                , movingRowAngle = moveAngle}
        , Cmd.none)
-}
