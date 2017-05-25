module FragmentShaderPicker exposing (fragmentShader)

import Math.Vector3 exposing (Vec3)
import WebGL exposing (Shader)


fragmentShader : Shader {} { u | shade : Float } { vcolor : Vec3 }
fragmentShader =
    [glsl|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]
