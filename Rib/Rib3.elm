module Rib3 exposing (..)

import Arithmetic exposing(isEven)


type N1 = MinRibRow
type N3 = MaxRibRow

type RibRow = N1 | N2 | N3

size: Int
size = 3

sizeEven: Bool
sizeEven = isEven size

halfSize: Int
halfSize = size // 2

ribRowToInt: RibRow -> Int
ribRowToInt ribRow =
  case ribRow of
    N1 ->
     1
    N2 ->
     2
    N3 ->
     3

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
