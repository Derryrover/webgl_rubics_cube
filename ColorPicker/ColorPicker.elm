module ColorPicker exposing(..)

import Rib
import Color
import List
import BlockBuilder
import BlockModel
import BlockModelDirection
import Maybe
import Dict

items = 6 * Rib.size * Rib.size

range: List Int
range =  List.range 1 items

colorsPossible = 16 ^ 6

step = ((colorsPossible //  items+1) - 1) --(items-1) -- we do not want black and white


colorsPossibleToRgb: Int -> (Int, Int, Int)
colorsPossibleToRgb i =
  let
    r = i // (16^4)
    g = (i // (16^2))%(16^2)
    b = i % (16^2)
  in
    (r, g, b)

mapFunction: Int -> Color.Color
mapFunction i =
  let
    amount = i * step
    (r,g,b) = colorsPossibleToRgb amount
  in
    Color.rgb r g b


colors = List.map mapFunction range


foldFunc: BlockModel.XYZColorRow -> (List Color.Color, List BlockModel.XYZColorRow) -> (List Color.Color, List BlockModel.XYZColorRow)
foldFunc xyzColorRow (colors, rows) =
  let
    xColor = xyzColorRow.colors.x
    (xColor2, colors2) =
      case xColor of
        Nothing ->
          (Nothing, colors)
        Just _ ->
          (List.head colors, List.drop 1 colors)
    yColor = xyzColorRow.colors.y
    (yColor2, colors3) =
      case yColor of
        Nothing ->
          (Nothing, colors2)
        Just _ ->
          (List.head colors2, List.drop 1 colors2)
    zColor = xyzColorRow.colors.z
    (zColor2, colors4) =
      case zColor of
        Nothing ->
          (Nothing, colors3)
        Just _ ->
          (List.head colors3, List.drop 1 colors3)
    newXYZColorRow =
      { rows = xyzColorRow.rows
      , colors = {x=xColor2,y=yColor2,z=zColor2}}
  in
    (colors4,(newXYZColorRow::rows))

allBlocksPicker: List BlockModel.XYZColorRow
allBlocksPicker = --BlockBuilder.allBlocks
  let (_, rows) = List.foldl foldFunc (colors ,[]) BlockBuilder.allBlocks
  in rows

faceMapFunc: BlockModel.XYZColorRow -> List (BlockModel.Face, Color.Color)
faceMapFunc {rows, colors} =
  let
    x =
      case colors.x of
        Nothing ->
          []
        Just colorX ->
          if rows.x == 1 then
            [({rows=rows, direction=BlockModel.Left}, colorX)]
          else if rows.x == Rib.size then
            [({rows=rows, direction=BlockModel.Right}, colorX)]
          else
            []
    y =
      case colors.y of
        Nothing ->
          []
        Just colorY ->
          if rows.y == 1 then
            [({rows=rows, direction=BlockModel.Top}, colorY)]
          else if rows.y == Rib.size then
            [({rows=rows, direction=BlockModel.Down}, colorY)]
          else
            []
    z =
      case colors.z of
        Nothing ->
          []
        Just colorZ ->
          if rows.z == 1 then
            [({rows=rows, direction=BlockModel.Back}, colorZ)]
          else if rows.z == Rib.size then
            [({rows=rows, direction=BlockModel.Front}, colorZ)]
          else
            []
  in
    List.concat [x,y,z]


allBlocksFaces: List (BlockModel.Face, Color.Color)
allBlocksFaces = List.concat (List.map faceMapFunc allBlocksPicker)


colorDict: Dict.Dict String BlockModel.Face
colorDict =
  let
    empty = Dict.empty
  in
    List.foldl (\ (face, color) dict -> Dict.insert (toString color) face dict) empty allBlocksFaces

getByColor str = Dict.get str colorDict


