module BlockToMatrixCoordinates exposing(..)

import Rib exposing(..)
import BlockModel exposing(Row)

-- [1,2,3] to [-1,0,1]
-- [1,2,3,4] to [-2,-1,1,2]
toMatrix: Row -> Row
toMatrix row =
  let
    unevenResult = row - halfSize - 1
  in
    if sizeEven && unevenResult >= 0 then
      unevenResult + 1
    else
      unevenResult

fromMatrix: Row -> Row
fromMatrix row =
  if sizeEven && row > 0 then
      row + halfSize
    else
      row + halfSize + 1

