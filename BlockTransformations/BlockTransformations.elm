module BlockTransformations exposing(filterRowFromBlocks,excludeRowFromBlocks,turnXRight,turnRow)

import BlockModel exposing(Block, Row, Axis, Turn, Move, mapBlockRow)
import List

import BlockToMatrixCoordinates exposing(toMatrix, fromMatrix)

filterRowSingleBlock: Row -> Axis -> Block -> Bool
filterRowSingleBlock row axis block =
  case axis of
    BlockModel.X ->
      row == block.x.row
    BlockModel.Y ->
      row == block.y.row
    BlockModel.Z ->
      row == block.z.row


filterRowFromBlocks: Row -> Axis -> List Block -> List Block
filterRowFromBlocks row axis list = List.filter (filterRowSingleBlock row axis) list

excludeRowFromBlocks: Row -> Axis -> List Block -> List Block
excludeRowFromBlocks row axis list = List.filter (\item-> not (filterRowSingleBlock row axis item)) list



turnXRight: Block -> Block
turnXRight block =
  { x = {row = block.x.row     , color = block.x.color }
  , y = {row = block.z.row * -1, color = block.z.color }
  , z = {row = block.y.row     , color = block.y.color } }

turnXLeft block =
  { x = {row = block.x.row     , color = block.x.color }
  , y = {row = block.z.row     , color = block.z.color }
  , z = {row = block.y.row * -1, color = block.y.color } }

turnYRight block =
  { x = {row = block.z.row     , color = block.z.color }
  , y = {row = block.y.row     , color = block.y.color }
  , z = {row = block.x.row * -1, color = block.x.color } }

turnYLeft block =
  { x = {row = block.z.row * -1, color = block.z.color }
  , y = {row = block.y.row     , color = block.y.color }
  , z = {row = block.x.row     , color = block.x.color } }

turnZRight block =
  { x = {row = block.y.row * -1, color = block.y.color }
  , y = {row = block.x.row     , color = block.x.color }
  , z = {row = block.z.row     , color = block.z.color } }

turnZLeft block =
  { x = {row = block.y.row     , color = block.y.color }
  , y = {row = block.x.row * -1, color = block.x.color }
  , z = {row = block.z.row     , color = block.z.color } }


turnSingleBlock: Axis -> Turn -> Block -> Block
turnSingleBlock axis turn block =
  case axis of
    BlockModel.X ->
      case turn of
        BlockModel.Right ->
          turnXRight block
        BlockModel.Left  ->
          turnXLeft  block
    BlockModel.Y ->
      case turn of
        BlockModel.Right ->
          turnYRight block
        BlockModel.Left ->
          turnYLeft  block
    BlockModel.Z ->
      case turn of
        BlockModel.Right ->
          turnZRight block
        BlockModel.Left ->
          turnZLeft  block

turnListOfBlocks: Axis -> Turn -> List Block -> List Block
turnListOfBlocks axis turn blocks = List.map (turnSingleBlock axis turn) blocks

turnRow: Move-> List Block -> List Block
turnRow {row, turn, axis} blocks =
  let
    leave    = excludeRowFromBlocks row axis blocks
    update   = filterRowFromBlocks row axis blocks
    matrix   = List.map (mapBlockRow toMatrix) update
    updated  = turnListOfBlocks axis turn matrix
    unMatrix = List.map (mapBlockRow fromMatrix) updated
  in
    List.concat [leave, unMatrix]

