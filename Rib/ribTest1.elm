module Rib exposing (..)

import Arithmetic exposing(isEven)

type EdgeRibRow = N1 | N3
type CenterRibRow = N2

type RibRow = EdgeRibRow | CenterRibRow

size: Int
size = 3

sizeEven: Bool
sizeEven = isEven size

halfSize: Int
halfSize = size // 2


ribRowToInt: RibRow -> Int
ribRowToInt rib =
  case rib of
    EdgeRibRow ->
     1
    CenterRibRow ->
     centerRibRowToInt CenterRibRow

centerRibRowToInt: CenterRibRow -> Int
centerRibRowToInt rib =
  case rib of
    N2 ->
     2

edgeRibRowToInt: EdgeRibRow -> Int
edgeRibRowToInt rib =
  case rib of
    N1 ->
     1
    N3->
     3

{-
intToRibRow: Int -> RibRow
intToRibRow ribRow =
    if      ribRow == 1 then
      N1
    else if ribRow == 2 then
      N2
    else if ribRow == 3 then
      N3
    else
      N1
-}
