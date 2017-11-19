module SpinCubeGl exposing(..)

import WebGL exposing (Entity,entity)
import VertexShader exposing(vertexShader)
import FragmentShader exposing(fragmentShader)

import VertexShaderPicker
import FragmentShaderPicker
import Uniforms exposing(uniforms, uniformsRow, uniformsNoShade)

import BlockToVertex
import BlockTransformations

import MainModel exposing (Model)

sceneSpinRow: Model -> List Entity
sceneSpinRow model =
  let
    {row,turn,axis} = model.lastMove
    blocks = BlockTransformations.filterRowFromBlocks row axis model.cube
    vertexes = List.concat (List.map BlockToVertex.singleBlockToVertexList blocks)
    triangles = WebGL.triangles vertexes
  in
    --[ render vertexShader fragmentShader triangles (uniformsRow model.angleX model.angleY model.movingRowAngle axis turn) ]
    [ entity vertexShader fragmentShader triangles (uniformsRow model.rotationMatrix model.movingRowAngle axis turn) ]


sceneRest: Model -> List Entity
sceneRest model =
  let
    {row,turn,axis} = model.lastMove
    blocks = BlockTransformations.excludeRowFromBlocks row axis model.cube
    vertexes = List.concat (List.map BlockToVertex.singleBlockToVertexList blocks)
    triangles = WebGL.triangles vertexes
  in
    --[ render vertexShader fragmentShader triangles (uniforms model.angleX model.angleY ) ]
    [ entity vertexShader fragmentShader triangles (uniforms model.rotationMatrix ) ]


scenePicker: Model -> List Entity
scenePicker model =
  let
    blocks = model.picker
    vertexes = List.concat (List.map BlockToVertex.singleBlockToVertexList blocks)
    triangles = WebGL.triangles vertexes
  in
    [ entity VertexShaderPicker.vertexShader FragmentShaderPicker.fragmentShader triangles (uniformsNoShade model.rotationMatrix ) ]
