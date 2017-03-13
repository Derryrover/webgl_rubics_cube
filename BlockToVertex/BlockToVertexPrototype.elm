module BlockToVertexPrototype exposing(protoAllFaces,ribLengthBLock)

import Rib

import Basics

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 as Matrix4

import BlockToVertexModel exposing(Face, ColorFace, ColorBlackFaces, AllSideFaces, mapFace, mapColorFace, mapColorBlackFaces, mapAllSideFaces)
import BlockToVertexCoordinates exposing(preProtoColorFace, preProtoBlackFace)

-- webgl coordinate constants to transform discreet coordinates to floats
wegGlDimension = 1
lowerBorderCube = -wegGlDimension
upperBorderCube =  wegGlDimension
ribLengthCube = upperBorderCube - lowerBorderCube
ribLengthBLock = (toFloat ribLengthCube) / (toFloat Rib.size)

scale = 1/(toFloat Rib.size)
scaleFunction = (Matrix4.transform (Matrix4.makeScale3 scale scale scale) )

preProtoColorBlackFaces: ColorBlackFaces
preProtoColorBlackFaces =
  { black = preProtoBlackFace
  , color = preProtoColorFace }

protoColorBlackFaces: ColorBlackFaces
protoColorBlackFaces = mapColorBlackFaces scaleFunction preProtoColorBlackFaces

preProtoAllFaces: AllSideFaces
preProtoAllFaces =
  { front      = protoColorBlackFaces
  , back       = mapColorBlackFaces (Matrix4.transform (Matrix4.makeRotate        Basics.pi  (vec3 0 1 0))) protoColorBlackFaces
  , left       = mapColorBlackFaces (Matrix4.transform (Matrix4.makeRotate (  0.5*Basics.pi) (vec3 0 1 0))) protoColorBlackFaces
  , right      = mapColorBlackFaces (Matrix4.transform (Matrix4.makeRotate ( -0.5*Basics.pi) (vec3 0 1 0))) protoColorBlackFaces
  , top        = mapColorBlackFaces (Matrix4.transform (Matrix4.makeRotate (  0.5*Basics.pi) (vec3 1 0 0))) protoColorBlackFaces
  , down       = mapColorBlackFaces (Matrix4.transform (Matrix4.makeRotate ( -0.5*Basics.pi) (vec3 1 0 0))) protoColorBlackFaces }


protoAllFaces: AllSideFaces
protoAllFaces =
  let
    shift = lowerBorderCube - (ribLengthBLock/2)
  in
    mapAllSideFaces (Matrix4.transform (Matrix4.makeTranslate3 shift shift shift)) preProtoAllFaces

