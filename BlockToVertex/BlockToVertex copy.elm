module BlockToVertex exposing(..)

import Rib
import BlockModel exposing(Row)
import Options
import List
import Maybe
import Math.Vector3 exposing (Vec3, vec3)
import Vertex exposing (Vertex)
import Color exposing (Color, toRgb, red, orange, yellow, green, blue, white, purple, black)
import CubeFace exposing(face)




-- webgl coordinate constants to transform discreet coordinates to floats
lowerBorderCube = -1
upperBorderCube = 1
ribLengthCube = upperBorderCube - lowerBorderCube
ribLengthBLock = (toFloat ribLengthCube) / (toFloat Rib.size)

colorFaceSize = 0.75
colorFaceEdge = ribLengthBLock * (1-colorFaceSize) / 2

----------------------------------------------------
-- one row needs upperborder and lowerborder
rowToTuple: BlockModel.Row -> (Int, Int)
rowToTuple row = (row-1, row)


------------------------------------------------------------------------------------------
-- 2 borders combined to 2d surface
-- used by create3dTuples
create2dTuples: (Int,Int) -> (Int,Int) -> ((Int,Int),(Int,Int),(Int,Int),(Int,Int))
create2dTuples (near1,far1) (near2,far2) =
  ((near1,near2)
  ,(far1 ,near2)
  ,(far1 ,far2 )
  ,(near1,far2 ))

type alias Tuple3d = (Float, Float, Float)

-- transform single coordinate to float
intToFloat: Int -> Float
intToFloat int =
  let float = toFloat int
  in lowerBorderCube + (ribLengthBLock * float)

create3dTuplesNew:  (Int,Int) -> (Int,Int) -> (Int,Int) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d)
create3dTuplesNew (nearX,farX) (nearY,farY) (nearZ,farZ) =
  let
    nX = intToFloat nearX
    fX = intToFloat farX
    nY = intToFloat nearY
    fY = intToFloat farY
    nZ = intToFloat nearY
    fZ = intToFloat farY
  in
   ( (nX, nY, nZ)
   , (fX, nY, nZ)
   , (fX, fY, nZ)
   , (nX, fY, nZ)
   , (nX, nY, fZ)
   , (fX, nY, fZ)
   , (fX, fY, fZ)
   , (nX, fY, fZ) )

{-

 /8-7
4-3/6
1-2/

-}
from3dTupleGetFront: (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
from3dTupleGetFront (n1,n2,n3,n4,n5,n6,n7,n8) = (n1,n2,n3,n4)

from3dTupleGetBack: (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
from3dTupleGetBack (n1,n2,n3,n4,n5,n6,n7,n8) = (n5,n6,n7,n8)

from3dTupleGetRight: (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
from3dTupleGetRight (n1,n2,n3,n4,n5,n6,n7,n8) = (n2,n3,n7,n6)

from3dTupleGetLeft: (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
from3dTupleGetLeft (n1,n2,n3,n4,n5,n6,n7,n8) = (n1,n4,n8,n5)

from3dTupleGetTop: (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
from3dTupleGetTop (n1,n2,n3,n4,n5,n6,n7,n8) = (n4,n3,n7,n8)

from3dTupleGetBottom: (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
from3dTupleGetBottom (n1,n2,n3,n4,n5,n6,n7,n8) = (n1,n2,n6,n5)


---------------------------------------------------------------------------

shrinkFaceX:(Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
shrinkFaceX ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3),(x4,y4,z4)) = --colorFaceEdge
    ( (x1, y1+colorFaceEdge, z1+colorFaceEdge)
    , (x2, y2+colorFaceEdge, z2-colorFaceEdge)
    , (x3, y3-colorFaceEdge, z3-colorFaceEdge)
    , (x4, y4-colorFaceEdge, z4+colorFaceEdge) )

shrinkFaceY:(Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
shrinkFaceY ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3),(x4,y4,z4)) = --colorFaceEdge
    ( (x1+colorFaceEdge, y1, z1+colorFaceEdge)
    , (x2+colorFaceEdge, y2, z2-colorFaceEdge)
    , (x3-colorFaceEdge, y3, z3-colorFaceEdge)
    , (x4-colorFaceEdge, y4, z4+colorFaceEdge) )
-------------------------------------------------------------------

blockToVertexList: BlockModel.Block -> List ( Vertex, Vertex, Vertex )
blockToVertexList
  let
    xTuple  = rowToTuple block.x.row
    yTuple  = rowToTuple block.y.row
    zTuple  = rowToTuple block.z.row
    tuple3d = create3dTuplesNew xTuple yTuple zTuple
    front   = from3dTupleGetFront  tuple3d
    back    = from3dTupleGetBack   tuple3d
    right   = from3dTupleGetRight  tuple3d
    left    = from3dTupleGetLeft   tuple3d
    top     = from3dTupleGetTop    tuple3d
    bottom  = from3dTupleGetBottom tuple3d

    xVertices =
      case block.x.color of
        Just colorX ->
          if block.x.row === 1 then
          [from3dTupleGetLeft xVertices,]
        Nothing ->
          []
    yVertices =
      case block.y.color of
        Just colorY ->
          [(colorY,(createVertices xTuple yTuple zTuple BlockModel.Y))]
        Nothing ->
          []
    zVertices =
      case block.z.color of
        Just colorZ ->
          [(colorZ,(createVertices xTuple yTuple zTuple BlockModel.Z))]
        Nothing ->
          []
  in
    List.concat [xVertices, yVertices, zVertices]





---------------------------------------------------------------------------------
-- combine border tuples to 3d coordinate tuples
create3dTuples: (Int,Int) -> (Int,Int) -> (Int,Int) -> BlockModel.Axis -> ((Int,Int,Int),(Int,Int,Int),(Int,Int,Int),(Int,Int,Int))
create3dTuples tupleX tupleY tupleZ axis =
  case axis of
    BlockModel.X ->
       let
         ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) = create2dTuples tupleY tupleZ
         x = if Tuple.first tupleX == 0 then 0 else Rib.size
       in
        ((x,a1,a2), (x,b1,b2), (x,c1,c2), (x,d1,d2))
    BlockModel.Y ->
      let
         ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) = create2dTuples tupleX tupleZ
         y = if Tuple.first tupleY == 0 then 0 else Rib.size
      in
        ((a1,y,a2), (b1,y,b2), (c1,y,c2), (d1,y,d2))
    BlockModel.Z ->
      let
         ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) = create2dTuples tupleX tupleY
         z = if Tuple.first tupleZ == 0 then 0 else Rib.size
       in
         ((a1,a2,z), (b1,b2,z), (c1,c2,z), (d1,d2,z))
-----------------------------------------------------------------------------------------------

-- transform items to Vec3




coordinate3dToFloat: (Int,Int,Int) -> (Float,Float,Float)
coordinate3dToFloat (a,b,c) =
 ( intToFloat a
 , intToFloat b
 , intToFloat c )


tuplesCoordinate3dToFloat: ((Int,Int,Int),(Int,Int,Int),(Int,Int,Int),(Int,Int,Int)) -> ((Float,Float,Float),(Float,Float,Float),(Float,Float,Float),(Float,Float,Float))
tuplesCoordinate3dToFloat (a,b,c,d) =
 ( coordinate3dToFloat a
 , coordinate3dToFloat b
 , coordinate3dToFloat c
 , coordinate3dToFloat d )


intTulip4ToVecTulip4: ((Float,Float,Float),(Float,Float,Float),(Float,Float,Float),(Float,Float,Float)) -> (Vec3,Vec3,Vec3,Vec3)
intTulip4ToVecTulip4 ( (a,b,c), (d,e,f), (g,h,i), (j,k,l) ) =
 ( vec3 a b c,
   vec3 d e f,
   vec3 g h i,
   vec3 j k l  )
--------------------------------------------------------------------
-- combine all from above block
createVertices: (Int,Int) -> (Int,Int) -> (Int,Int) -> BlockModel.Axis -> (Vec3,Vec3,Vec3,Vec3)
createVertices tupleX tupleY tupleZ axis =
   let
     integers = create3dTuples tupleX tupleY tupleZ axis
     floats = tuplesCoordinate3dToFloat integers
   in
     intTulip4ToVecTulip4 floats

------------------------------------------------------------------------------------------------
singleBlockToVecColorList: BlockModel.Block -> List (Color.Color, (Vec3,Vec3,Vec3,Vec3))
singleBlockToVecColorList block =
  let
    xTuple = rowToTuple block.x.row
    yTuple = rowToTuple block.y.row
    zTuple = rowToTuple block.z.row
    xVertices =
      case block.x.color of
        Just colorX ->
          [(colorX,(createVertices xTuple yTuple zTuple BlockModel.X))]
        Nothing ->
          []
    yVertices =
      case block.y.color of
        Just colorY ->
          [(colorY,(createVertices xTuple yTuple zTuple BlockModel.Y))]
        Nothing ->
          []
    zVertices =
      case block.z.color of
        Just colorZ ->
          [(colorZ,(createVertices xTuple yTuple zTuple BlockModel.Z))]
        Nothing ->
          []
  in
    List.concat [xVertices, yVertices, zVertices]

singleBLockToVertexMapFunction: (BlockModel.Color, (Vec3,Vec3,Vec3,Vec3)) -> List ( Vertex, Vertex, Vertex )
singleBLockToVertexMapFunction (color,(a,b,c,d)) = face color a b c d

singleBLockToVertexList: List (BlockModel.Color, (Vec3,Vec3,Vec3,Vec3)) -> List ( Vertex, Vertex, Vertex )
singleBLockToVertexList list =
  List.concat (List.map singleBLockToVertexMapFunction list)


finalMapFunction: BlockModel.Block -> List ( Vertex, Vertex, Vertex )
finalMapFunction block =
  singleBLockToVertexList (singleBlockToVecColorList block)


blockListToVertexList: List BlockModel.Block -> List ( Vertex, Vertex, Vertex )
blockListToVertexList blocks =
  List.concat ( List.map finalMapFunction blocks)

