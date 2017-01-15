module BlockModel2 exposing (..)

import Maybe
import Color as ColorImport


type Axis = X | Y | Z
type Turn = Left | Right

type alias Color =  ColorImport.Color

type alias Row = Int


type alias BlockRow =
  { row : Row
  , color : Maybe Color }

type alias Block =
    { x : BlockRow
    , y : BlockRow
    , z : BlockRow }
