module Rib exposing (..)

import Arithmetic exposing(isEven)


type N1 = MinRibRow
type N2 = CenterRibRow
type N3 = MaxRibRow


{-
type RibRow = N1 | N2 | N3
-}


type RibEdge = RibEdgeMin N1 | RibEdgeMax N3
type RibCenter = RibCenterCenter N2
type RibRow = RibRowEdge RibEdge | RibRowCenter RibCenter


size: Int
size = 3

sizeEven: Bool
sizeEven = isEven size

halfSize: Int
halfSize = size // 2


ribRowToInt: RibRow -> Int
ribRowToInt ribRow =
  case ribRow of
    (RibRowEdge ribEdge) ->
      ribEdgeToInt ribEdge
    (RibRowCenter ribCenter) ->
      ribCenterToInt ribCenter


ribEdgeToInt: RibEdge -> Int
ribEdgeToInt rib =
  case rib of
    (RibEdgeMin _) ->
      1
    (RibEdgeMax _) ->
      3

ribCenterToInt: RibCenter -> Int
ribCenterToInt rib =
  case rib of
    (RibCenterCenter _) ->
      2


intToRibRow: Int -> RibRow
intToRibRow ribRow =
    if      ribRow == 1 then
      RibRowEdge (RibEdgeMin MinRibRow)
    else if ribRow == 2 then
      RibRowCenter (RibCenterCenter CenterRibRow)
    else if ribRow == 3 then
      RibRowEdge (RibEdgeMax MaxRibRow)
    else
      RibRowEdge (RibEdgeMin MinRibRow)

