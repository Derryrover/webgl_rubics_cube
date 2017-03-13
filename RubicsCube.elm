port module RubicsCube exposing(..)

import Html
import AnimationFrame
import Html.Attributes exposing (style, width, height, id)

-- for drag
import Html.Events exposing (on)
import Html.Events exposing(onClick)
import Json.Decode as Decode
import Mouse exposing (Position)

import WebGL exposing (Entity,entity,antialias,alpha,depth)

import Rib
import BlockModel
import BlockTransformations

import MainModel exposing (Model, init)
import MainAngle exposing(calculateAngle,calculateMoveRowAngle)
import SpinCubeGl exposing(sceneSpinRow, sceneRest, scenePicker)
import MainMessage exposing(Msg(..))
import MainUpdateFrame exposing(updateFrame)
import MainHtmlButton exposing(buttonCreator)

import CalculateViewFromDrag

import RotationDirectionFromDrag
import ColorPicker
import Maybe

port sendCoordinates : String -> Cmd msg
port listenForColors : (String -> msg) -> Sub msg

type alias Drag =
    { start : Position
    , current : Position
    }


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , subscriptions = subscriptions
    , update = update }

subscriptions : Model -> Sub Msg
subscriptions model  =
  case model.dragging of
    True ->
       Sub.batch [ AnimationFrame.diffs Frame, Mouse.moves DragAt, Mouse.ups DragEnd, listenForColors MoveColor]
    False ->
       Sub.batch [ AnimationFrame.diffs Frame, listenForColors MoveColor]
{-
subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
-}

-- Update
update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DragStart position ->
      case model.movingRow of
        False ->
          ({ model | formerDragX = toFloat position.x
                   , formerDragY = toFloat position.y
                   , beginDragX = toFloat position.x
                   , beginDragY = toFloat position.y
                   , dragging = True} , sendCoordinates ((toString position.x) ++ "," ++ (toString position.y))  )--Cmd.none)
        True ->
          (model,Cmd.none)
      --(model,Cmd.none)
    DragAt position ->
      case model.dragging of
        True->
          case model.colorPicked of
            Just color ->
              case model.movingRow of
                True ->
                  (model,Cmd.none)
                False ->
                  case ((abs(model.beginDragX - (toFloat position.x))) + (abs(model.beginDragY - (toFloat position.y)))) > 80 of
                     False ->
                       (model,Cmd.none)
                     True ->
                       (updateLastMoveByColor model color (model.beginDragY - (toFloat position.y)) (model.beginDragX - (toFloat position.x)) ,Cmd.none)
              {-
              ({ model | rotationMatrix = CalculateViewFromDrag.calculateView model.rotationMatrix ((model.formerDragY - (toFloat position.y))/180) ((model.formerDragX - (toFloat position.x))/180)
                       , formerDragX = toFloat position.x
                       , formerDragY = toFloat position.y } , Cmd.none)
                       -}
            Nothing ->
              ({ model | rotationMatrix = CalculateViewFromDrag.calculateView model.rotationMatrix ((model.formerDragY - (toFloat position.y))/180) ((model.formerDragX - (toFloat position.x))/180)
                       , formerDragX = toFloat position.x
                       , formerDragY = toFloat position.y } , Cmd.none)

        False ->
          (model,Cmd.none)
    DragEnd position ->
      ({ model | formerDragX = 0
               , formerDragY = 0
               , dragging = False} , Cmd.none)
      --(model,Cmd.none)
    Frame dt ->
      case model.movingRow of
        False ->
          (model,Cmd.none)
          {-
          ({ model | angle           = calculateAngle model.angle dt
                   , angleHorizontal = calculateAngle model.angleHorizontal dt
                   , angleVertical   = calculateAngle model.angleVertical   dt } , Cmd.none)-}
        True  ->
          updateFrame model dt
    MoveRow move ->
        ({ model | lastMove = move
                 , movingRow = True },Cmd.none)
    EndMove ->
        ({ model | cube = BlockTransformations.turnRow  model.lastMove model.cube
                 , moves= model.lastMove :: model.moves
                 , movingRowAngle  = 0
                 , movingRow = False
                 , colorPicked = Nothing
                 , dragging = False},Cmd.none)
    MoveColor color ->
      --(updateLastMoveByColor model color,Cmd.none)
      ({ model | colorPicked = ColorPicker.getByColor color},Cmd.none)


updateLastMoveByColor model face x y =
    { model | lastMove = RotationDirectionFromDrag.calculateMove model.rotationMatrix x y face
            , movingRow = True }

(=>) = (,)

view: Model -> Html.Html Msg
view model =
  --Html.div [onMouseDown, style ["height" => "100%"] ]
  Html.div [ onMouseDown, style ["height" => "100%"] ]
    [ Html.div [style ["height" => "100%"]]
          --[ WebGL.toHtml [width 800, height 800] (List.concat[(sceneSpinRow model), (sceneRest model)]) ]
          --[ WebGL.toHtml [onMouseDown, width 800, height 800, style ["height" => "100%"]] (List.concat[(sceneSpinRow model), (sceneRest model)]) ]
          --[ WebGL.toHtml [id "js_cubicle_canvas", onMouseDown,width 800, height 800, style ["height" => "100%"]] (List.concat[(sceneSpinRow model), (sceneRest model)])
          [ WebGL.toHtml [id "js_cubicle_canvas",width 800, height 800, style ["height" => "100%", "display"=>"block","margin" => "auto"]] (List.concat[(sceneSpinRow model), (sceneRest model)])
          , WebGL.toHtmlWith [ WebGL.alpha False, WebGL.depth 1 ] [width 800, height 800, style ["height" => "100%", "display" => "none"], id "js_color_picker"] (scenePicker model) ]
          --, WebGL.toHtml [width 800, height 800, style ["height" => "100%"], id "js_color_picker"] (scenePicker model) ]
     {-
    , Html.div [] (List.map (buttonCreator BlockModel.X BlockModel.Clock) Rib.range)
    , Html.div [] (List.map (buttonCreator BlockModel.Y BlockModel.Clock) Rib.range)
    , Html.div [] (List.map (buttonCreator BlockModel.Z BlockModel.Clock) Rib.range)
    , Html.button
    [onClick (MoveColor "RGBA 109 9 142 1")]
    [Html.text "MoveColor"]
    -}
    ]

onMouseDown : Html.Attribute Msg
onMouseDown =
  on "mousedown" (Decode.map DragStart Mouse.position)

