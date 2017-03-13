module BlockToVertexModel exposing(..)

import Math.Vector3 exposing (Vec3, vec3)
import BlockModel
import List
import Maybe

import Vertex exposing (Vertex)


type alias Face = (Vec3,Vec3,Vec3,Vec3)

type alias FaceVertex = (Vertex,Vertex,Vertex,Vertex)
type alias Face3Vertex = (Vertex,Vertex,Vertex)

mapFace: (Vec3->Vec3) -> Face -> Face
mapFace f (a,b,c,d) =
  (f a, f b, f c, f d)


type alias ColorFace =
  { center: Face
  , top: Face
  , down: Face
  , left: Face
  , right: Face }

mapColorFace: (Vec3->Vec3) -> ColorFace -> ColorFace
mapColorFace f {center,top,down,left,right} =
  { center = mapFace f center
  , top    = mapFace f top
  , down   = mapFace f down
  , left   = mapFace f left
  , right  = mapFace f right }


type alias ColorBlackFaces =
  {  black: Face
  ,  color: ColorFace }

mapColorBlackFaces: (Vec3->Vec3) -> ColorBlackFaces -> ColorBlackFaces
mapColorBlackFaces f {black,color} =
  { black = mapFace f black
  , color = mapColorFace f color }


type alias AllSideFaces =
  { front : ColorBlackFaces
  , back  : ColorBlackFaces
  , left  : ColorBlackFaces
  , right : ColorBlackFaces
  , top   : ColorBlackFaces
  , down  : ColorBlackFaces }

mapAllSideFaces: (Vec3->Vec3) -> AllSideFaces -> AllSideFaces
mapAllSideFaces f rec =
  { front       = mapColorBlackFaces f rec.front
  , back        = mapColorBlackFaces f rec.back
  , left        = mapColorBlackFaces f rec.left
  , right       = mapColorBlackFaces f rec.right
  , top         = mapColorBlackFaces f rec.top
  , down        = mapColorBlackFaces f rec.down }

getVertexFromDirection: AllSideFaces -> BlockModel.Direction  -> ColorBlackFaces
getVertexFromDirection allSide dir =
  case dir of
    BlockModel.Top ->
      {  black= allSide.down.black
      ,  color= allSide.top.color }
    BlockModel.Down ->
      {  black= allSide.top.black
      ,  color= allSide.down.color }
    BlockModel.Left ->
      {  black= allSide.right.black
      ,  color= allSide.left.color }
    BlockModel.Right ->
      {  black= allSide.left.black
      ,  color= allSide.right.color }
    BlockModel.Front ->
      {  black= allSide.back.black
      ,  color= allSide.front.color }
    BlockModel.Back ->
      {  black= allSide.front.black
      ,  color= allSide.back.color }

type alias BlackFacesAndSingleColorFace =
  { black: List Face
  , color: Maybe Face }

toBlackFacesAndSingleColorFace: ColorBlackFaces -> BlackFacesAndSingleColorFace
toBlackFacesAndSingleColorFace cBF =
  { black= [cBF.black, cBF.color.top, cBF.color.down, cBF.color.left, cBF.color.right]
  , color= Just cBF.color.center }

type alias XYZColorsListBlack =
  { x: Maybe Face
  , y: Maybe Face
  , z: Maybe Face
  , black: List Face }

type alias VertexListPlusColors =
  { x: Maybe Face
  , y: Maybe Face
  , z: Maybe Face
  , black: List FaceVertex }

type alias VertexListPlusFace3Colors =
  { x: Maybe Face
  , y: Maybe Face
  , z: Maybe Face
  , black: List Face3Vertex }

{-
getVertexDirection: BlockModel.Direction -> AllSideFaces -> ColorBlackFaces
getVertexDirection dir allSide =
  case dir of
    BlockModel.Top ->
      allSide.top
    BlockModel.Down ->
      allSide.down
    BlockModel.Left ->
      allSide.left
    BlockModel.Right ->
      allSide.right
    BlockModel.Front ->
      allSide.front
    BlockModel.Back ->
      allSide.back
-}

