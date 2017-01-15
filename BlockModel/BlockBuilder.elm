module BlockBuilder exposing (allBlocks)

import List
import List.Extra exposing(lift3)
import Maybe
import Color

import Rib exposing(size, range)
import BlockModel exposing (Row, BlockRow, Block)

coordinates: List (Row, Row, Row)
coordinates =  lift3 (\x y z -> (x,y,z)) range range range

getXRow: Row -> BlockRow
getXRow row =
  if row == 1 then
    {row = row, color = Just Color.red}
  else if row == size then
    {row = row, color = Just Color.green}
  else
    {row = row, color = Nothing }

getYRow: Row -> BlockRow
getYRow row =
  if row == 1 then
    {row = row, color = Just Color.blue}
  else if row == size then
    {row = row, color = Just Color.orange}
  else
    {row = row, color = Nothing }

getZRow: Row -> BlockRow
getZRow row =
  if row == 1 then
    {row = row, color = Just Color.yellow}
  else if row == size then
    {row = row, color = Just Color.purple}
  else
    {row = row, color = Nothing }

mapSingleBlock: (Row, Row, Row) ->  Block
mapSingleBlock tuple =
  let
    (x,y,z) = tuple
  in
    { x = getXRow x
    , y = getYRow y
    , z = getZRow z }


allBlocks: List Block
allBlocks = List.map mapSingleBlock coordinates

