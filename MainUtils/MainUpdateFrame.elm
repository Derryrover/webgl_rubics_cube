module MainUpdateFrame exposing(..)

import CommandToMessage
import MainModel exposing (Model, init)
import MainAngle exposing(calculateAngle,calculateMoveRowAngle)
import SpinCubeGl exposing(sceneSpinRow, sceneRest)

import MainMessage exposing(Msg(..))

import CalculateViewFromDrag

updateFrame model dt =
  let
    moveAngle = --calculateMoveRowAngle model.movingRowAngle dt
      case model.movingRow of
        False ->
          0.0
        True ->
          calculateMoveRowAngle model.movingRowAngle dt
    --viewAngle = model.angle--calculateAngle model.angle dt
    xKeys0 =
      case model.arrowLeft of
        True ->
          case model.arrowRight of
            True ->
              0.0
            False ->
              0.08
        False ->
          case model.arrowRight of
            True ->
              -0.08
            False ->
              0.0
    yKeys0 =
      case model.arrowUp of
        True ->
          case model.arrowDown of
            True ->
              0.0
            False ->
              0.08
        False ->
          case model.arrowDown of
            True ->
              -0.08
            False ->
              0.0
    xKeys =
      if yKeys0 == 0 then
        xKeys0
      else if xKeys0 > 0 then
        sqrt((xKeys0 * xKeys0 ) / 2)
      else
        -(sqrt((xKeys0 * xKeys0 ) / 2))
    yKeys =
      if xKeys0 == 0 then
        yKeys0
      else if yKeys0 > 0 then
        sqrt((yKeys0 * yKeys0 ) / 2)
      else
        -(sqrt((yKeys0 * yKeys0 ) / 2))
  in
    case (moveAngle > (0.5*Basics.pi)) of
      True ->
        ({ model | --angle = viewAngle
                  movingRowAngle = 0
                  ,rotationMatrix = CalculateViewFromDrag.calculateView model.rotationMatrix yKeys xKeys } -- only needed for keys, not for drag
        , CommandToMessage.message EndMove)
      False ->
        ({ model | --angle = viewAngle
                 movingRowAngle = moveAngle
                 ,rotationMatrix = CalculateViewFromDrag.calculateView model.rotationMatrix yKeys xKeys } -- only needed for keys, not for drag
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
