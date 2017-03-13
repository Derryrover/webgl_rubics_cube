module BlockToVertexCoordinates exposing(..)

import BlockToVertexModel exposing(Face, ColorFace, ColorBlackFaces, AllSideFaces, mapFace, mapColorFace, mapColorBlackFaces, mapAllSideFaces)
import Math.Vector3 exposing (Vec3, vec3)

colorFaceSize = 0.75

-- See RubicsVertexSide PNG image in Root folder for coordinates explained
preProtoColorFace: ColorFace
preProtoColorFace =
  let x = colorFaceSize
  in
    { center =
        ( vec3 -x -x  1
        , vec3  x -x  1
        , vec3  x  x  1
        , vec3 -x  x  1 )
    , top    =
        ( vec3 -1  x  1
        , vec3  1  x  1
        , vec3  1  1  1
        , vec3 -1  1  1 )
    , down   =
        ( vec3 -1 -1  1
        , vec3  1 -1  1
        , vec3  1 -x  1
        , vec3 -1 -x  1 )
    , left   =
        ( vec3 -1 -x  1
        , vec3 -x -x  1
        , vec3 -x  x  1
        , vec3 -1  x  1 )
    , right  =
        ( vec3  x -x  1
        , vec3  1 -x  1
        , vec3  1  x  1
        , vec3  x  x  1 ) }

preProtoBlackFace: Face
preProtoBlackFace =
  ( vec3 -1 -1  1
  , vec3  1 -1  1
  , vec3  1  1  1
  , vec3 -1  1  1 )
