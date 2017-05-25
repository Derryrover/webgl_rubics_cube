module CubeFace exposing (face,toFace3Vertex)

import Math.Vector3 exposing (Vec3, vec3)
import Vertex exposing (Vertex)
import Color exposing (Color, toRgb)

import List

import BlockToVertexModel


face : Color -> BlockToVertexModel.Face -> Vec3 -> BlockToVertexModel.FaceVertex
face rawColor (one,two,three,four) normal =
  let
    rgb = toRgb rawColor
    r = (toFloat rgb.red   / 255)
    g = (toFloat rgb.green / 255)
    b = (toFloat rgb.blue  / 255)
    colorVec3 = vec3 r g b
  in
    (Vertex colorVec3 one normal, Vertex colorVec3 two normal, Vertex colorVec3 three normal, Vertex colorVec3 four normal)


toFace3Vertex: BlockToVertexModel.FaceVertex -> List BlockToVertexModel.Face3Vertex
toFace3Vertex (a,b,c,d) =
  [ (a,b,c)
  , (c,d,a) ]


------------------------------------------------------------------
{-
faceOld : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
faceOld rawColor a b c d =
    let
        color =
            let
                c =
                    toRgb rawColor
            in
                vec3
                    (toFloat c.red / 255)
                    (toFloat c.green / 255)
                    (toFloat c.blue / 255)

        vertex position =
            Vertex color position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]
-}
