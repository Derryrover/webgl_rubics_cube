module Cube exposing (cube)

import Math.Vector3 exposing (vec3)
import Vertex exposing (Vertex)
import CubeFace exposing (face)
import Color exposing (green, blue, yellow, red, purple, orange)
import WebGL exposing (Drawable)


-- MESHES - create a cube in which each vertex has a position and color


cube : Drawable Vertex
cube =
    let
        rft =
            vec3 1 1 1

        -- right, front, top
        lft =
            vec3 -1 1 1

        -- left,  front, top
        lbt =
            vec3 -1 -1 1

        rbt =
            vec3 1 -1 1

        rbb =
            vec3 1 -1 -1

        rfb =
            vec3 1 1 -1

        lfb =
            vec3 -1 1 -1

        lbb =
            vec3 -1 -1 -1
    in
        WebGL.Triangle
            << List.concat
        <|
            [ face green rft rfb rbb rbt
              -- right
            , face blue rft rfb lfb lft
              -- front
            , face yellow rft lft lbt rbt
              -- top
            , face red rfb lfb lbb rbb
              -- bottom
            , face purple lft lfb lbb lbt
              -- left
            , face orange rbt rbb lbb lbt
              -- back
            ]
