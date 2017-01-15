module BlockBuilderCoordinates exposing(..)

import List
import List.Extra exposing(lift3)
import Rib exposing(size)
import BlockModel exposing(Row)


oneDimension: List Row
oneDimension =  List.range 1 size

coordinates: List (Row, Row, Row)
coordinates =  lift3 (\x y z -> (x,y,z)) oneDimension oneDimension oneDimension
