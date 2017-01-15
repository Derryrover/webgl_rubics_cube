module BlockModel exposing (..)

import Maybe
import Color as ColorImport


type Axis = X | Y | Z
type Turn = Left | Right

type alias Color =  ColorImport.Color

type alias Row = Int

type alias Move =
  { row: Row
  , turn: Turn
  , axis: Axis }


type alias BlockRow =
  { row : Row
  , color : Maybe Color }

type alias Block =
    { x : BlockRow
    , y : BlockRow
    , z : BlockRow }

mapBlockRow: (Row-> Row) -> Block -> Block
mapBlockRow fn block =
  let
    x = block.x
    y = block.y
    z = block.z
    xRow = fn x.row
    yRow = fn y.row
    zRow = fn z.row
  in
     { x = { row=xRow, color=x.color}
     , y = { row=yRow, color=y.color}
     , z = { row=zRow, color=z.color}}
