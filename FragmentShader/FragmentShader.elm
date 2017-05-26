module FragmentShader exposing (fragmentShader)

import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import WebGL exposing (Shader)


fragmentShader : Shader {} { u | shade : Float } { vColor : Vec3, vTransformedNormal: Vec3, vPosition : Vec4 }
fragmentShader =
    [glsl|

precision mediump float;

uniform float shade; // needed?

varying vec3 vColor;
varying vec3 vTransformedNormal;
varying vec4 vPosition;

void main () {

    if ( vColor == vec3(0.0,0.0,0.0)) {
      gl_FragColor = vec4(0.0,0.0,0.0,1.0);
    } else {
       vec3 normal = normalize(vTransformedNormal);
      vec3 eyeDirection = normalize( vPosition.xyz - vec3(0.0,0.0,5.0) );

      vec3 ambientLight = vec3(0.28, 0.28, 0.28);//vec3(0.6, 0.6, 0.6);
      vec3 pointLightingColor = vec3(0.65, 0.65, 0.65);//vec3(0.7, 0.7, 0.7);
      vec3 pointLightingSpecularColor = vec3(0.28, 0.28, 0.28);
      float materialShininess = 31.5;//2.0;


      vec3 lightDirectionPoint = vec3( 0.0,  0.0,  5.0);
      vec3 lightDirectionSpec1 = vec3( 0.0,  0.0,  5.0);
      vec3 lightDirectionSpec2 = vec3( 1.0,  5.0, -2.0);
      vec3 lightDirectionSpec3 = vec3( 5.0, -3.0, -3.0);
      vec3 lightDirectionSpec4 = vec3( 0.0, -7.0,  1.0);

      vec3 relativeLightDirectionPoint = normalize( vPosition.xyz - lightDirectionPoint);
      vec3 relativeLightDirectionSpec1 = normalize( vPosition.xyz - lightDirectionSpec1);
      vec3 relativeLightDirectionSpec2 = normalize( vPosition.xyz - lightDirectionSpec2);
      vec3 relativeLightDirectionSpec3 = normalize( vPosition.xyz - lightDirectionSpec3);
      vec3 relativeLightDirectionSpec4 = normalize( vPosition.xyz - lightDirectionSpec4);

      vec3 reflectionDirection1 = reflect(-relativeLightDirectionSpec1, normal); // original
      vec3 reflectionDirection2 = reflect(-relativeLightDirectionSpec2, normal); // original
      vec3 reflectionDirection3 = reflect(-relativeLightDirectionSpec3, normal); // original
      vec3 reflectionDirection4 = reflect(-relativeLightDirectionSpec4, normal); // original

      float specularLightWeighting1 = pow(max(dot(reflectionDirection1, eyeDirection), 0.0), materialShininess);
      float specularLightWeighting2 = pow(max(dot(reflectionDirection2, eyeDirection), 0.0), materialShininess);
      float specularLightWeighting3 = pow(max(dot(reflectionDirection3, eyeDirection), 0.0), materialShininess);
      float specularLightWeighting4 = pow(max(dot(reflectionDirection4, eyeDirection), 0.0), materialShininess);


      float directionalLightWeighting = max(dot(normal, -relativeLightDirectionPoint), 0.0);


      highp vec3 lightWeighting = ambientLight + pointLightingColor * directionalLightWeighting;
      gl_FragColor = vec4((vColor * lightWeighting + specularLightWeighting1 * pointLightingSpecularColor
                                                   + specularLightWeighting2 * pointLightingSpecularColor
                                                   + specularLightWeighting3 * pointLightingSpecularColor
                                                   + specularLightWeighting4 * pointLightingSpecularColor), 1.0);




    }


}

|]
