module MainModel exposing(..)

import BlockModel
import BlockBuilder
import Math.Matrix4 as Matrix4
import Math.Vector3 exposing (Vec3, vec3, normalize)
import Basics
import ColorPicker
import Maybe

-- MODEL
type alias Model =
  { cube: List BlockModel.XYZColorRow
  , picker: List BlockModel.XYZColorRow
  , angle: Float
  , moves: List BlockModel.Move
  , movingRow: Bool
  , lastMove: BlockModel.Move
  , movingRowAngle: Float
  , dragging: Bool
  , beginDragX: Float
  , beginDragY: Float
  , formerDragX: Float
  , formerDragY: Float
  , rotationMatrix: Matrix4.Mat4
  , colorPicked: Maybe BlockModel.Face
  , arrowUp: Bool
  , arrowDown: Bool
  , arrowLeft: Bool
  , arrowRight: Bool
  }

init: (Model, Cmd msg)
init =
  ({ cube =  BlockBuilder.allBlocks
  , picker = ColorPicker.allBlocksPicker
  , angle = 0
  , moves = []
  , movingRow = False
  , lastMove = {row=1, turn=BlockModel.Clock, axis=BlockModel.X} --need to initiate it to something otherwise the type would by Maybe Move
  , movingRowAngle = 0
  , dragging = False
  , beginDragX = 0
  , beginDragY = 0
  , formerDragX = 0
  , formerDragY = 0
  , rotationMatrix = Matrix4.makeRotate (Basics.pi * 0.3) (vec3 1 1 0)
  , colorPicked = Nothing
  , arrowUp = False
  , arrowDown = False
  , arrowLeft = False
  , arrowRight = False
  --Matrix4.makeRotate (Basics.pi * 0.29) (vec3 1 1 0)
  --Matrix4.makeRotate (Basics.pi*0.5) (vec3 1 1 0)
  --Matrix4.makeRotate (Basics.pi*0.25) (vec3 1 (Basics.acos (0.5)) 0) --((Basics.acos (0.5))/Basics.pi)
  --Matrix4.makeRotate (Basics.pi*0.25) (vec3 1 (Basics.acos (0.5)) 0)
  --Matrix4.makeRotate (Basics.acos (0.5)) (vec3 1 (Basics.acos (0.5)) 0)
  --Matrix4.makeRotate (Basics.pi*0.25) (vec3 1 (Basics.acos (0.5)) 0)
  --Matrix4.makeRotate (Basics.acos (0.5)) (normalize(vec3 1 1 0))
  --Matrix4.makeRotate (Basics.pi*0.3) (normalize(vec3 1 1 0))
  --Matrix4.makeRotate (Basics.pi*0.25) (vec3 1 0 0)--(normalize(vec3 1 1 1))
  --Matrix4.makeRotate (Basics.pi*0.25) (vec3 0.6 0.6 0.6)
  --Matrix4.makeRotate (Basics.pi*0.25) (normalize(vec3 1 1 1))
  --Matrix4.makeRotate (Basics.pi*0.25) (vec3 1 1.2 0)
  --Matrix4.mul (Matrix4.identity) (Matrix4.makeRotate (Basics.pi*0.25) (vec3 1 1.2 0))
  --Matrix4.identity

  },Cmd.none)
