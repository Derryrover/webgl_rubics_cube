module VertexShader exposing (vertexShader)

import Math.Vector3 exposing (Vec3)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Shader)


vertexShader : Shader { attr | position : Vec3, color : Vec3 } { unif | rotation : Mat4, perspective : Mat4, camera : Mat4 } { vcolor : Vec3 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
varying vec3 vcolor;
void main () {
    gl_Position = perspective * camera * rotation * vec4(position, 1.0);
    vcolor = color;
}

|]
