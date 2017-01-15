module BlockToVertex exposing(..)

import Rib
import BlockModel exposing(Row)
import Options

import Basics
import List
import Maybe
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 as Matrix4
import Vertex exposing (Vertex)
import Color exposing (Color, toRgb, red, orange, yellow, green, blue, white, purple, black)
import CubeFace exposing(face)




-- webgl coordinate constants to transform discreet coordinates to floats
lowerBorderCube = -1
upperBorderCube =  1
ribLengthCube = upperBorderCube - lowerBorderCube
ribLengthBLock = (toFloat ribLengthCube) / (toFloat Rib.size)

colorFaceSize = 0.75
scale = 1/(toFloat Rib.size)
scaleFunction = (Matrix4.transform (Matrix4.makeScale3 scale scale scale) )

type alias SimpleFace = (Vec3,Vec3,Vec3,Vec3)
type alias Face = (SimpleFace,SimpleFace,SimpleFace,SimpleFace,SimpleFace)

mapSimpleFace: (Vec3->Vec3) -> SimpleFace -> SimpleFace
mapSimpleFace f (a,b,c,d) =
  (f a, f b, f c, f d)


mapFace: (Vec3->Vec3) -> Face -> Face
mapFace f (a,b,c,d,e) =
  ( mapSimpleFace f a
  , mapSimpleFace f b
  , mapSimpleFace f c
  , mapSimpleFace f d
  , mapSimpleFace f e )

type alias AllFaces =
  { front : Face
  , back  : Face
  , left  : Face
  , right : Face
  , top   : Face
  , down  : Face }

mapAllFaces: (Vec3->Vec3) -> AllFaces -> AllFaces
mapAllFaces f rec =
  { front       = mapFace f rec.front
  , back        = mapFace f rec.back
  , left        = mapFace f rec.left
  , right       = mapFace f rec.right
  , top         = mapFace f rec.top
  , down      = mapFace f rec.down }

preProtoFace: Face
preProtoFace =
  (
  ( vec3 -1 -1 -1
  , vec3  -colorFaceSize -1 -1
  , vec3  -colorFaceSize  1 -1
  , vec3 -1  1 -1 )
  ,
  ( vec3 colorFaceSize -1 -1
  , vec3  1 -1 -1
  , vec3  1  1 -1
  , vec3 colorFaceSize  1 -1 )
  ,
  ( vec3 -1 -1 -1
  , vec3  1 -1 -1
  , vec3  1  -colorFaceSize -1
  , vec3 -1  -colorFaceSize -1 )
  ,
  ( vec3 -1 colorFaceSize -1
  , vec3  1 colorFaceSize -1
  , vec3  1  1 -1
  , vec3 -1  1 -1 )
  , -- center
  ( vec3 -colorFaceSize -colorFaceSize -1
  , vec3  colorFaceSize -colorFaceSize -1
  , vec3  colorFaceSize  colorFaceSize -1
  , vec3 -colorFaceSize  colorFaceSize -1 )
  )

protoFace: Face
protoFace = mapFace scaleFunction preProtoFace

preProtoAllFaces: AllFaces
preProtoAllFaces =
  { front      = mapFace (Matrix4.transform (Matrix4.makeRotate Basics.pi (vec3 0 1 0))) protoFace
  , back       = protoFace
  , left       = mapFace (Matrix4.transform (Matrix4.makeRotate (  0.5*Basics.pi) (vec3 0 1 0))) protoFace
  , right      = mapFace (Matrix4.transform (Matrix4.makeRotate ( -0.5*Basics.pi) (vec3 0 1 0))) protoFace
  , top        = mapFace (Matrix4.transform (Matrix4.makeRotate (  0.5*Basics.pi) (vec3 1 0 0))) protoFace
  , down       = mapFace (Matrix4.transform (Matrix4.makeRotate ( -0.5*Basics.pi) (vec3 1 0 0))) protoFace }

protoAllFaces: AllFaces
protoAllFaces =
  let
    shift = -1 + (ribLengthBLock/2)
  in
    mapAllFaces (Matrix4.transform (Matrix4.makeTranslate3 shift shift shift)) preProtoAllFaces


createCubeFace: SimpleFace -> Color.Color -> List ( Vertex, Vertex, Vertex )
createCubeFace (a,b,c,d) color = face color a b c d

singleBlockToVertexList: BlockModel.Block -> List ( Vertex, Vertex, Vertex )
singleBlockToVertexList rec =
  let
    x = (toFloat (rec.x.row-1)) * ribLengthBLock
    y = (toFloat (rec.y.row-1)) * ribLengthBLock
    z = (toFloat (rec.z.row-1)) * ribLengthBLock
    translated = mapAllFaces (Matrix4.transform (Matrix4.makeTranslate3 x y z)) protoAllFaces
    (fa,fb,fc,fd,fe) = translated.front
    (ba,bb,bc,bd,be) = translated.back
    (la,lb,lc,ld,le) = translated.left
    (ra,rb,rc,rd,re) = translated.right
    (ta,tb,tc,td,te) = translated.top
    (da,db,dc,dd,de) = translated.down
    colorX = case rec.x.color of
      Nothing ->
        black
      Just color ->
        color
    colorY = case rec.y.color of
      Nothing ->
        black
      Just color ->
        color
    colorZ = case rec.z.color of
      Nothing ->
        black
      Just color ->
        color
    listPreFront =
      [ createCubeFace fa black
      , createCubeFace fb black
      , createCubeFace fc black
      , createCubeFace fd black ]
    listPreBack =
      [ createCubeFace ba black
      , createCubeFace bb black
      , createCubeFace bc black
      , createCubeFace bd black ]
    listPreLeft =
      [ createCubeFace la black
      , createCubeFace lb black
      , createCubeFace lc black
      , createCubeFace ld black ]
    listPreRight =
      [ createCubeFace ra black
      , createCubeFace rb black
      , createCubeFace rc black
      , createCubeFace rd black ]
    listPreTop =
      [ createCubeFace ta black
      , createCubeFace tb black
      , createCubeFace tc black
      , createCubeFace td black ]
    listPreDown =
      [ createCubeFace da black
      , createCubeFace db black
      , createCubeFace dc black
      , createCubeFace dd black ]
    listFront =
      if rec.z.row /= 1 then
        (createCubeFace fe colorZ)::listPreFront
      else
        (createCubeFace fe black )::listPreFront
    listBack =
      if rec.z.row == 1 then
        (createCubeFace be colorZ)::listPreBack
      else
        (createCubeFace be black )::listPreBack
    listRight =
      if rec.x.row /= 1 then
        (createCubeFace re colorX)::listPreRight
      else
        (createCubeFace re black )::listPreRight
    listLeft =
      if rec.x.row == 1 then
        (createCubeFace le colorX)::listPreLeft
      else
        (createCubeFace le black )::listPreLeft
    listTop =
      if rec.y.row /= 1 then
        (createCubeFace te colorY)::listPreTop
      else
        (createCubeFace te black )::listPreTop
    listDown =
      if rec.y.row == 1 then
        (createCubeFace de colorY)::listPreDown
      else
        (createCubeFace de black )::listPreDown
  in
    List.concat( List.concat [listFront, listBack,listLeft, listRight,listTop, listDown])
{-

{ front :(fa,fb,fc,fd,fe)
    , back  :(ba,bb,bc,bd,be)
    , left  :(la,lb,lc,ld,le)
    , right :(ra,rb,rc,rd,re)
    , top   :(ta,tb,tc,td,te)
    , down:(ba,bb,bc,bd,be) } = mapAllFaces (Matrix4.transform (Matrix4.makeTranslate3 x y z)) protoAllFaces

type alias AllFaces =
  { front: Face
  , frontSmall: Face
  , back: Face
  , backSmall: Face
  , left: Face
  , leftSmall: Face
  , right: Face
  , rightSmall: Face
  , top: Face
  , topSmall: Face
  , down: Face
  , downSmall: Face }

mapAllFaces: (Vec3->Vec3) -> AllFaces -> AllFaces
mapAllFaces f rec =
  { front       = mapFace f rec.front
  , frontSmall  = mapFace f rec.frontSmall
  , back        = mapFace f rec.back
  , backSmall   = mapFace f rec.backSmall
  , left        = mapFace f rec.left
  , leftSmall   = mapFace f rec.leftSmall
  , right       = mapFace f rec.right
  , rightSmall  = mapFace f rec.rightSmall
  , top         = mapFace f rec.top
  , topSmall    = mapFace f rec.topSmall
  , down      = mapFace f rec.down
  , downSmall = mapFace f rec.downSmall }

preProtoFace: Face
preProtoFace =
  ( vec3 -1 -1 -1
  , vec3  1 -1 -1
  , vec3  1  1 -1
  , vec3 -1  1 -1 )


preProtoFaceSmall: Face
preProtoFaceSmall =
  ( vec3 -colorFaceSize -colorFaceSize -1
  , vec3  colorFaceSize -colorFaceSize -1
  , vec3  colorFaceSize  colorFaceSize -1
  , vec3 -colorFaceSize  colorFaceSize -1 )




protoFace: Face
protoFace = mapFace scaleFunction preProtoFace

protoFaceSmall: Face
protoFaceSmall = mapFace scaleFunction preProtoFaceSmall

preProtoAllFaces: AllFaces
preProtoAllFaces =
  { front      = mapFace (Matrix4.transform (Matrix4.makeRotate Basics.pi (vec3 0 1 0))) protoFace
  , frontSmall = mapFace (Matrix4.transform (Matrix4.makeRotate Basics.pi (vec3 0 1 0))) protoFaceSmall
  , back       = protoFace
  , backSmall  = protoFaceSmall
  , left       = mapFace (Matrix4.transform (Matrix4.makeRotate (  0.5*Basics.pi) (vec3 0 1 0))) protoFace
  , leftSmall  = mapFace (Matrix4.transform (Matrix4.makeRotate (  0.5*Basics.pi) (vec3 0 1 0))) protoFaceSmall
  , right      = mapFace (Matrix4.transform (Matrix4.makeRotate ( -0.5*Basics.pi) (vec3 0 1 0))) protoFace
  , rightSmall = mapFace (Matrix4.transform (Matrix4.makeRotate ( -0.5*Basics.pi) (vec3 0 1 0))) protoFaceSmall
  , top        = mapFace (Matrix4.transform (Matrix4.makeRotate (  0.5*Basics.pi) (vec3 1 0 0))) protoFace
  , topSmall   = mapFace (Matrix4.transform (Matrix4.makeRotate (  0.5*Basics.pi) (vec3 1 0 0))) protoFaceSmall
  , down     = mapFace (Matrix4.transform (Matrix4.makeRotate ( -0.5*Basics.pi) (vec3 1 0 0))) protoFace
  , downSmall= mapFace (Matrix4.transform (Matrix4.makeRotate ( -0.5*Basics.pi) (vec3 1 0 0))) protoFaceSmall }

protoAllFaces: AllFaces
protoAllFaces =
  let
    shift = -1 + scale
  in
    mapAllFaces (Matrix4.transform (Matrix4.makeTranslate3 shift shift shift)) preProtoAllFaces


createCubeFace: Face -> Color.Color -> List ( Vertex, Vertex, Vertex )
createCubeFace (a,b,c,d) color = face color a b c d

singleBlockToVertexList: BlockModel.Block -> List ( Vertex, Vertex, Vertex )
singleBlockToVertexList rec =
  let
    x = (toFloat rec.x.row) * scale
    y = (toFloat rec.y.row) * scale
    z = (toFloat rec.z.row) * scale
    translated = mapAllFaces (Matrix4.transform (Matrix4.makeTranslate3 x y z)) protoAllFaces
    colorX = case rec.x.color of
      Nothing ->
        black
      Just color ->
        color
    colorY = case rec.y.color of
      Nothing ->
        black
      Just color ->
        color
    colorZ = case rec.z.color of
      Nothing ->
        black
      Just color ->
        color
    listX =
      if (x==1) then
      [ createCubeFace translated.left       black
      , createCubeFace translated.leftSmall  colorX
      , createCubeFace translated.right      black
      , createCubeFace translated.rightSmall black ]
      else
      [ createCubeFace translated.left       black
      , createCubeFace translated.leftSmall  black
      , createCubeFace translated.right      black
      , createCubeFace translated.rightSmall colorX ]
    listY =
      if (x==1) then
      [ createCubeFace translated.top         black
      , createCubeFace translated.topSmall    colorX
      , createCubeFace translated.down      black
      , createCubeFace translated.downSmall black ]
      else
      [ createCubeFace translated.top         black
      , createCubeFace translated.topSmall    black
      , createCubeFace translated.down      black
      , createCubeFace translated.downSmall colorX ]
    listZ =
      if (x==1) then
      [ createCubeFace translated.front         black
      , createCubeFace translated.frontSmall    colorX
      , createCubeFace translated.back          black
      , createCubeFace translated.backSmall     black ]
      else
      [ createCubeFace translated.front         black
      , createCubeFace translated.frontSmall    black
      , createCubeFace translated.back          black
      , createCubeFace translated.backSmall     colorX ]
  in
    List.concat( List.concat [listX, listY, listZ])

-}
-- face color a b c d
-- face color a b c d
{-
----------------------------------------------------
-- one row needs upperborder and lowerborder
rowToTuple= BlockModel.Row -> (Int, Int)
rowToTuple row = (row-1, row)


------------------------------------------------------------------------------------------
-- 2 borders combined to 2d surface
-- used by create3dTuples
create2dTuples= (Int,Int) -> (Int,Int) -> ((Int,Int),(Int,Int),(Int,Int),(Int,Int))
create2dTuples (near1,far1) (near2,far2) =
  ((near1,near2)
  ,(far1 ,near2)
  ,(far1 ,far2 )
  ,(near1,far2 ))

type alias Tuple3d = (Float, Float, Float)

-- transform single coordinate to float
intToFloat= Int -> Float
intToFloat int =
  let float = toFloat int
  in lowerBorderCube + (ribLengthBLock * float)

create3dTuplesNew=  (Int,Int) -> (Int,Int) -> (Int,Int) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d)
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
from3dTupleGetFront= (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
from3dTupleGetFront (n1,n2,n3,n4,n5,n6,n7,n8) = (n1,n2,n3,n4)

from3dTupleGetBack= (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
from3dTupleGetBack (n1,n2,n3,n4,n5,n6,n7,n8) = (n5,n6,n7,n8)

from3dTupleGetRight= (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
from3dTupleGetRight (n1,n2,n3,n4,n5,n6,n7,n8) = (n2,n3,n7,n6)

from3dTupleGetLeft= (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
from3dTupleGetLeft (n1,n2,n3,n4,n5,n6,n7,n8) = (n1,n4,n8,n5)

from3dTupleGetTop= (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
from3dTupleGetTop (n1,n2,n3,n4,n5,n6,n7,n8) = (n4,n3,n7,n8)

from3dTupleGetdown= (Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
from3dTupleGetdown (n1,n2,n3,n4,n5,n6,n7,n8) = (n1,n2,n6,n5)


---------------------------------------------------------------------------

shrinkFaceX=(Tuple3d,Tuple3d,Tuple3d,Tuple3d) -> (Tuple3d,Tuple3d,Tuple3d,Tuple3d)
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
    down  = from3dTupleGetdown tuple3d

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
-}

