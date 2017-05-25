module MainMessage exposing(..)

import BlockModel
import Mouse exposing (Position)
import Keyboard

type Msg =
    Frame Float
  | MoveRow BlockModel.Move
  | EndMove
  | DragStart Position
  | DragAt Position
  | DragEnd Position
  | MoveColor String
  | KeyMsg Keyboard.KeyCode
