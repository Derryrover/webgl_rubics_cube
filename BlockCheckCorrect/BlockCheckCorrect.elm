module BlockCheckCorrect exposing(..)

import BlockModel exposing(Block,Axis, Color)
import Maybe
import List

type alias CheckItemMaybe =
  { row:Int
  , axis:Axis
  , color: Maybe Color }

mapFunction: Block -> List CheckItemMaybe
mapFunction block =
  let
    x = if block.x.color == Nothing then [] else [{row=block.x.coordinate, axis=BlockModel.X, color=block.x.color}]
    y = if block.y.color == Nothing then [] else [{row=block.y.coordinate, axis=BlockModel.Y, color=block.y.color}]
    z = if block.z.color == Nothing then [] else [{row=block.z.coordinate, axis=BlockModel.Z, color=block.z.color}]
  in
    List.concat [x,y,z]


foldFunction: CheckItemMaybe -> List CheckItemMaybe -> List CheckItemMaybe
foldFunction item list =
  if List.member item list then
    list
  else
    item::list



checkCorrect: List Block -> Bool
checkCorrect blocks =
  let
    colorList  = List.concat(List.map mapFunction blocks)
    redList    = List.filter (\item -> item.color == Just BlockModel.Red   ) colorList
    orangeList = List.filter (\item -> item.color == Just BlockModel.Orange) colorList
    yellowList = List.filter (\item -> item.color == Just BlockModel.Yellow) colorList
    greenList  = List.filter (\item -> item.color == Just BlockModel.Green ) colorList
    blueList   = List.filter (\item -> item.color == Just BlockModel.Blue  ) colorList
    whiteList  = List.filter (\item -> item.color == Just BlockModel.White ) colorList
    red    = List.length (List.foldl foldFunction [] redList   ) == 1
    orange = List.length (List.foldl foldFunction [] orangeList) == 1
    yellow = List.length (List.foldl foldFunction [] yellowList) == 1
    green  = List.length (List.foldl foldFunction [] greenList ) == 1
    blue   = List.length (List.foldl foldFunction [] blueList  ) == 1
    white  = List.length (List.foldl foldFunction [] whiteList ) == 1
  in
    red && orange && yellow && green && blue && white

