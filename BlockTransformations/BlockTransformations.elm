module BlockTransformations exposing(filterRowSingleBlock,filterRowFromBlocks,excludeRowFromBlocks,turnRow)

import BlockModel exposing(XYZColorRow, Row, Axis, Turn, Move, mapXYZ)
import List

import BlockToMatrixCoordinates exposing(toMatrix, fromMatrix)

filterRowSingleBlock: Row -> Axis -> XYZColorRow -> Bool
filterRowSingleBlock row axis block =
  case axis of
    BlockModel.X ->
      row == block.rows.x
    BlockModel.Y ->
      row == block.rows.y
    BlockModel.Z ->
      row == block.rows.z


filterRowFromBlocks: Row -> Axis -> List XYZColorRow -> List XYZColorRow
filterRowFromBlocks row axis list = List.filter (filterRowSingleBlock row axis) list


excludeRowFromBlocks: Row -> Axis -> List XYZColorRow -> List XYZColorRow
excludeRowFromBlocks row axis list = List.filter (\item-> not (filterRowSingleBlock row axis item)) list



turnXClock: XYZColorRow -> XYZColorRow
turnXClock {rows,colors} =
  { rows   = { rows   | y =   rows.z * -1
                      , z =   rows.y }
  , colors = { colors | y = colors.z
                      , z = colors.y } }

turnXAnti: XYZColorRow -> XYZColorRow
turnXAnti {rows,colors} =
  { rows   = { rows   | y =   rows.z
                      , z =   rows.y * -1}
  , colors = { colors | y = colors.z
                      , z = colors.y } }


turnYClock: XYZColorRow -> XYZColorRow
turnYClock {rows,colors} =
  { rows   = { rows   | x =   rows.z
                      , z =   rows.x  * -1 }
  , colors = { colors | x = colors.z
                      , z = colors.x } }

turnYAnti: XYZColorRow -> XYZColorRow
turnYAnti {rows,colors} =
  { rows   = { rows   | x =   rows.z * -1
                      , z =   rows.x }
  , colors = { colors | x = colors.z
                      , z = colors.x } }


turnZClock: XYZColorRow -> XYZColorRow
turnZClock {rows,colors} =
  { rows   = { rows   | x =   rows.y * -1
                      , y =   rows.x }
  , colors = { colors | x = colors.y
                      , y = colors.x } }

turnZAnti: XYZColorRow -> XYZColorRow
turnZAnti {rows,colors} =
  { rows   = { rows   | x =   rows.y
                      , y =   rows.x * -1}
  , colors = { colors | x = colors.y
                      , y = colors.x } }



turnSingleBlock: Axis -> Turn -> XYZColorRow -> XYZColorRow
turnSingleBlock axis turn block =
  case axis of
    BlockModel.X ->
      case turn of
        BlockModel.Clock ->
          turnXClock block
        BlockModel.Anti  ->
          turnXAnti  block
    BlockModel.Y ->
      case turn of
        BlockModel.Clock ->
          turnYClock block
        BlockModel.Anti ->
          turnYAnti  block
    BlockModel.Z ->
      case turn of
        BlockModel.Clock ->
          turnZClock block
        BlockModel.Anti ->
          turnZAnti  block

turnListOfBlocks: Axis -> Turn -> List XYZColorRow -> List XYZColorRow
turnListOfBlocks axis turn blocks = List.map (turnSingleBlock axis turn) blocks


turnRow: Move-> List XYZColorRow -> List XYZColorRow
turnRow {row, turn, axis} blocks =
  let
    leave    = excludeRowFromBlocks row axis blocks
    update   = filterRowFromBlocks row axis blocks
    matrix   = List.map (\item-> { item | rows = mapXYZ   toMatrix item.rows}) update
    updated  = turnListOfBlocks axis turn matrix
    unMatrix = List.map (\item-> { item | rows = mapXYZ fromMatrix item.rows}) updated
  in
    List.concat [leave, unMatrix]
{-
-}
