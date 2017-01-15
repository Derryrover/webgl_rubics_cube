import Html
import AnimationFrame
import Html.Attributes exposing (width, height)
import Html.Events exposing(onClick)
import List
import Maybe
import Basics

import WebGL exposing (Renderable,render)
import VertexShader exposing(vertexShader)
import FragmentShader exposing(fragmentShader)
import Uniforms exposing(uniforms, uniformsRow)

import Arithmetic
import CommandToMessage

import Rib
import BlockModel
import BlockBuilder
import BlockToVertex
import BlockTransformations

--------------------------------------------------------
import Task

{-| A command to generate a message without performing any action.
This is useful for implementing components that generate events in the manner
of HTML elements, but where the event fires from within Elm code, rather than
by an external trigger.
-}
message : msg -> Cmd msg
message x =
  Task.perform identity (identity (Task.succeed x))

---------------------------------------------------

-- MESSAGES
type Msg =
    Frame Float
  | MoveRow BlockModel.Move
  | EndMove

-- MODEL
type alias Model =
  { cube: List BlockModel.Block
  , angle: Float
  , moves: List BlockModel.Move
  , movingRow: Bool
  , lastMove: BlockModel.Move
  , movingRowAngle: Float }

init: (Model, Cmd msg)
init =
  ({ cube = BlockBuilder.allBlocks
  , angle = 0
  , moves = []
  , movingRow = False
  , lastMove = {row=1, turn=BlockModel.Right, axis=BlockModel.X}
  , movingRowAngle = 0},Cmd.none)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , subscriptions = subscriptions
    , update = update }


subscriptions model  = Sub.batch [ AnimationFrame.diffs Frame]

-- Update

calculateAngle: Float -> Float -> Float
calculateAngle old delta =
  let
    new = old + (delta/ 5000)
  in
    Arithmetic.modFloat new (2*Basics.pi)

calculateMoveRowAngle: Float -> Float -> Float
calculateMoveRowAngle old delta =
    old + (delta/ 300)


update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Frame dt ->
      case model.movingRow of
        False ->
          ({ model | angle = calculateAngle model.angle dt} , Cmd.none)
        True  ->
          let
            newMoveRow = calculateMoveRowAngle model.movingRowAngle dt
          in
            case (newMoveRow > (0.5*Basics.pi)) of
              True ->
                ({ model | angle          = calculateAngle        model.angle dt
                         , movingRowAngle = 0}--0.5*Basics.pi}
                , CommandToMessage.message EndMove)--message EndMove)
                --Cmd.map (\n->EndMove) Cmd.none)
                --,Cmd EndMove)
              False ->
                ({ model | angle          = calculateAngle        model.angle dt
                         , movingRowAngle = calculateMoveRowAngle model.movingRowAngle dt}
                , Cmd.none)
    MoveRow move ->
        ({ model | lastMove = move --cube = BlockTransformations.turnRow  move model.cube
                 , movingRow = True },Cmd.none)
    EndMove ->
        ({ model | cube = BlockTransformations.turnRow  model.lastMove model.cube
                 , moves= model.lastMove :: model.moves
                 , movingRowAngle  = 0
                 , movingRow = False },Cmd.none)


buttonCreator: BlockModel.Axis -> BlockModel.Turn -> BlockModel.Row -> Html.Html Msg
buttonCreator axis turn row =
  Html.button
    [onClick (MoveRow {row=row, turn=turn, axis=axis})]
    [Html.text "Turn1XRight"]


view: Model -> Html.Html Msg
view model =
  Html.div []
    [ Html.div []
          --[ WebGL.toHtml [width 400, height 400] (scene model) ]
          [ WebGL.toHtml [width 400, height 400] (List.concat[(sceneSpinRow model), (sceneRest model)]) ]
    , Html.div [] (List.map (buttonCreator BlockModel.X BlockModel.Right) Rib.range)
    , Html.div [] (List.map (buttonCreator BlockModel.Y BlockModel.Right) Rib.range)
    , Html.div [] (List.map (buttonCreator BlockModel.Z BlockModel.Right) Rib.range)
    , Html.div [] [Html.text (toString model.movingRow)]]

{-
scene : Model -> List Renderable
scene model =
  let
    vertexes = BlockToVertex.blockListToVertexList model.cube
    triangles = WebGL.Triangle vertexes
  in
    [ render vertexShader fragmentShader triangles (uniforms model.angle) ]
-}

sceneSpinRow: Model -> List Renderable
sceneSpinRow model =
  let
    {row,turn,axis} = model.lastMove
    blocks = BlockTransformations.filterRowFromBlocks row axis model.cube
    vertexes = List.concat (List.map BlockToVertex.singleBlockToVertexList blocks)
    triangles = WebGL.Triangle vertexes
  in
    [ render vertexShader fragmentShader triangles (uniformsRow model.angle model.movingRowAngle axis turn) ]


sceneRest: Model -> List Renderable
sceneRest model =
  let
    {row,turn,axis} = model.lastMove
    blocks = BlockTransformations.excludeRowFromBlocks row axis model.cube
    vertexes = List.concat (List.map BlockToVertex.singleBlockToVertexList blocks)
    triangles = WebGL.Triangle vertexes
  in
    [ render vertexShader fragmentShader triangles (uniforms model.angle ) ]

