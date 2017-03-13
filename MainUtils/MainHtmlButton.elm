module MainHtmlButton exposing (..)

import Html
import Html.Attributes exposing (width, height)
import Html.Events exposing(onClick)
import BlockModel
import MainMessage exposing(Msg(..))

buttonCreator: BlockModel.Axis -> BlockModel.Turn -> BlockModel.Row -> Html.Html Msg
buttonCreator axis turn row =
  Html.button
    [onClick (MoveRow {row=row, turn=turn, axis=axis})]
    [Html.text "Turn"]
