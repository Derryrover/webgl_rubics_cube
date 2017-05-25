module VertexShader exposing (vertexShader)

import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Shader)


vertexShader : Shader { attr | position : Vec3, color : Vec3, normal : Vec3 } { unif | rotation : Mat4, perspective : Mat4, camera : Mat4 } { vColor : Vec3, vTransformedNormal : Vec3,  vPosition : Vec4 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 color;
attribute vec3 normal;

uniform mat4 rotation;
uniform mat4 perspective;
uniform mat4 camera;
varying vec3 vColor;
varying vec3 vTransformedNormal;
varying vec4 vPosition;

void main(void) {
  vColor = color;
  vPosition = rotation * vec4(position, 1.0);
  gl_Position = perspective * camera * vPosition;
  vec4 tempTransformedNormal = rotation * vec4(normal, 1.0);
  //vTransformedNormal = vec3(1.0,0.0,0.0);//tempTransformedNormal.xyz;
  vTransformedNormal = tempTransformedNormal.xyz;
}
|]
