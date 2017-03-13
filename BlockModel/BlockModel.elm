module BlockModel exposing
  ( Axis(..)
  , Turn(..)
  , Direction(..)
  , Color
  , Row
  , XYZ
  , mapXYZ
  , XYZRow
  , XYZColor
  , XYZColorRow
  , Move
  , Face )

import Maybe
import Color as ColorImport

type Axis = X | Y | Z
type Turn = Clock | Anti
type Direction = Top | Down | Left | Right | Front | Back
type alias Color =  ColorImport.Color
type alias Row = Int

type alias XYZ a =
  { x: a
  , y: a
  , z: a }

mapXYZ: (a->b) -> XYZ a -> XYZ b
mapXYZ f {x,y,z} =
  { x= f x
  , y= f y
  , z= f z }

type alias XYZRow = XYZ Row
type alias XYZColor = XYZ (Maybe Color)

type alias XYZColorRow =
  { rows: XYZRow
  , colors: XYZColor }

type alias Move =
  { row: Row
  , turn: Turn
  , axis: Axis }

type alias Face =
  { rows: XYZRow
  , direction: Direction }



