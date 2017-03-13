module BlockToVertex exposing(..)

import BlockToVertexModel
import BlockToVertexPrototype exposing(ribLengthBLock,protoAllFaces)

import Rib exposing(size, range)
import BlockModel
import BlockModelDirection
import BlockBuilder exposing(coordinates, allBlocks)
import Vertex exposing (Vertex)
import Color exposing (Color, toRgb, red, orange, yellow, green, blue, white, purple, black)
import CubeFace

import Basics
import List
import Maybe
import Dict

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 as Matrix4

-- calculate single cube from coordinates and protocube
xyzRowToAllFaces: BlockModel.XYZRow -> BlockToVertexModel.AllSideFaces
xyzRowToAllFaces {x,y,z} =
  let
    shift = ribLengthBLock
  in
    BlockToVertexModel.mapAllSideFaces (Matrix4.transform (Matrix4.makeTranslate3 (shift*(toFloat x)) (shift*(toFloat y)) (shift*(toFloat z)))) protoAllFaces

-- extract faces to be drawn from allFaces
xyzRowColorToListFaces: BlockModel.XYZRow -> BlockToVertexModel.XYZColorsListBlack
xyzRowColorToListFaces xyz =
  let
    allFaces = xyzRowToAllFaces xyz
    directionX = BlockModelDirection.getDirectionXAxis xyz.x
    directionY = BlockModelDirection.getDirectionYAxis xyz.y
    directionZ = BlockModelDirection.getDirectionZAxis xyz.z
    colorX = Maybe.map (BlockToVertexModel.getVertexFromDirection allFaces) directionX
    colorY = Maybe.map (BlockToVertexModel.getVertexFromDirection allFaces) directionY
    colorZ = Maybe.map (BlockToVertexModel.getVertexFromDirection allFaces) directionZ
    listColorX =
      case colorX of
        Nothing ->
          { black= [allFaces.left.black, allFaces.right.black]
          , color= Nothing }
        Just colorBlackX->
          BlockToVertexModel.toBlackFacesAndSingleColorFace colorBlackX
    listColorY =
      case colorY of
        Nothing ->
          { black= [allFaces.top.black, allFaces.down.black]
          , color= Nothing }
        Just colorBlackY->
          BlockToVertexModel.toBlackFacesAndSingleColorFace colorBlackY
    listColorZ =
      case colorZ of
        Nothing ->
          { black= [allFaces.front.black, allFaces.back.black]
          , color= Nothing }
        Just colorBlackZ->
          BlockToVertexModel.toBlackFacesAndSingleColorFace colorBlackZ
  in
    { x= listColorX.color
    , y= listColorY.color
    , z= listColorZ.color
    , black= List.concat[listColorX.black, listColorY.black, listColorZ.black] }


toVertexListPlusColors: BlockToVertexModel.XYZColorsListBlack -> BlockToVertexModel.VertexListPlusColors
toVertexListPlusColors colorList =
  { colorList | black= List.map (CubeFace.face black) colorList.black}

toVertexListPlusFace3Colors: BlockToVertexModel.VertexListPlusColors -> BlockToVertexModel.VertexListPlusFace3Colors
toVertexListPlusFace3Colors colorList =
  { colorList | black= List.concat (List.map CubeFace.toFace3Vertex colorList.black)}


xyzRowToVertexListPlusFace3Colors: BlockModel.XYZRow -> BlockToVertexModel.VertexListPlusFace3Colors
xyzRowToVertexListPlusFace3Colors xyz =
  let
    xyzColorsListBlack = xyzRowColorToListFaces xyz
    vertexListPlusColors = toVertexListPlusColors xyzColorsListBlack
    vertexListPlusFace3Colors = toVertexListPlusFace3Colors vertexListPlusColors
  in
    vertexListPlusFace3Colors

allFacesDict: Dict.Dict (BlockModel.Row,BlockModel.Row,BlockModel.Row) BlockToVertexModel.VertexListPlusFace3Colors
allFacesDict =
  let
    empty = Dict.empty
  in
    List.foldl (\ block dict -> Dict.insert (block.rows.x,block.rows.y,block.rows.z) (xyzRowToVertexListPlusFace3Colors block.rows) dict) empty allBlocks


getFromDict: BlockModel.XYZRow -> Maybe BlockToVertexModel.VertexListPlusFace3Colors
getFromDict {x,y,z} =
  Dict.get (x,y,z) allFacesDict

singleBlockToVertexList: BlockModel.XYZColorRow -> List BlockToVertexModel.Face3Vertex
singleBlockToVertexList {rows,colors} =
  let
    vertexListPlusFace3Colors = getFromDict rows
  in
    case vertexListPlusFace3Colors of
      Nothing ->
        []
      Just listPlus3Colors ->
        let
          xFace = Maybe.andThen (\face-> Maybe.map (\color->CubeFace.face color face) colors.x) listPlus3Colors.x
          yFace = Maybe.andThen (\face-> Maybe.map (\color->CubeFace.face color face) colors.y) listPlus3Colors.y
          zFace = Maybe.andThen (\face-> Maybe.map (\color->CubeFace.face color face) colors.z) listPlus3Colors.z
          xMList = Maybe.map (CubeFace.toFace3Vertex) xFace
          yMList = Maybe.map (CubeFace.toFace3Vertex) yFace
          zMList = Maybe.map (CubeFace.toFace3Vertex) zFace
          xList =
            case xMList of
              Nothing ->
                []
              Just xL ->
                xL
          yList =
            case yMList of
              Nothing ->
                []
              Just yL ->
                yL
          zList =
            case zMList of
              Nothing ->
                []
              Just zL ->
                zL
        in
          List.concat[xList,yList,zList, listPlus3Colors.black]








