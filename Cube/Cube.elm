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
            [ face green rft rfb rbb rbt (vec3 1 0 0)
              -- right
            , face blue rft rfb lfb lft (vec3 0 1 0)
              -- front
            , face yellow rft lft lbt rbt (vec3 0 0 -1)
              -- top
            , face red rfb lfb lbb rbb (vec3 0 0 1)
              -- bottom
            , face purple lft lfb lbb lbt (vec3 -1 0 0)
              -- left
            , face orange rbt rbb lbb lbt (vec3 0 -1 0)
              -- back
            ]
