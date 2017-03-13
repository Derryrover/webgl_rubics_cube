module MainMessage exposing(..)

import BlockModel
import Mouse exposing (Position)

type Msg =
    Frame Float
  | MoveRow BlockModel.Move
  | EndMove
  | DragStart Position
  | DragAt Position
  | DragEnd Position
  | MoveColor String
