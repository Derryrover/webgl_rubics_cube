module BlockModelDirection exposing(directionToInitialColor, xyzRowToXYZColor, getDirectionXAxis, getDirectionYAxis, getDirectionZAxis,xyzRowToXYZDirection)

import BlockModel
import Rib exposing(size)

import Color
import Maybe

directionToInitialColor: BlockModel.Direction -> BlockModel.Color
directionToInitialColor dir =
  case dir of
    BlockModel.Top ->
      --Color.orange
      Color.rgb 235 131 32
    BlockModel.Down ->
      --Color.red
      Color.rgb 223 29 20
    BlockModel.Left ->
      --Color.green
      Color.rgb 88 222 50
    BlockModel.Right ->
      --Color.blue
      Color.rgb 1 8 116
    BlockModel.Front ->
      --Color.white
      Color.rgb 193 197 197
    BlockModel.Back ->
      --Color.yellow
      Color.rgb 228 231 33

getDirectionXAxis: BlockModel.Row -> Maybe BlockModel.Direction
getDirectionXAxis row =
  if row == 1 then
    Just BlockModel.Right
  else if row == size then
    Just BlockModel.Left
  else
    Nothing

getDirectionYAxis: BlockModel.Row -> Maybe BlockModel.Direction
getDirectionYAxis row =
  if row == 1 then
    Just BlockModel.Top
  else if row == size then
    Just BlockModel.Down
  else
    Nothing

getDirectionZAxis: BlockModel.Row -> Maybe BlockModel.Direction
getDirectionZAxis row =
  if row == 1 then
    Just BlockModel.Back
  else if row == size then
    Just BlockModel.Front
  else
    Nothing

xyzRowToXYZColor: BlockModel.XYZRow ->  BlockModel.XYZColor
xyzRowToXYZColor xyz =
  let
    xDirection = getDirectionXAxis xyz.x
    yDirection = getDirectionYAxis xyz.y
    zDirection = getDirectionZAxis xyz.z
    xColor = Maybe.map directionToInitialColor xDirection
    yColor = Maybe.map directionToInitialColor yDirection
    zColor = Maybe.map directionToInitialColor zDirection
  in
    { x = xColor
    , y = yColor
    , z = zColor }

-- added for shaders
xyzRowToXYZDirection: BlockModel.XYZRow ->  BlockModel.XYZDirection
xyzRowToXYZDirection xyz =
  let
    xDirection = getDirectionXAxis xyz.x
    yDirection = getDirectionYAxis xyz.y
    zDirection = getDirectionZAxis xyz.z
  in
    { x = xDirection
    , y = yDirection
    , z = zDirection }



