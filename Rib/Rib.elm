module Rib exposing (..)

import Arithmetic exposing(isEven)
import List
import BlockModel exposing(Row)


size: Int
size = 3

sizeEven: Bool
sizeEven = isEven size

halfSize: Int
halfSize = size // 2

range: List Row
range =  List.range 1 size


