module BlockBuilder exposing (coordinates,allBlocks)

import List
import List.Extra exposing(lift3)

import Rib exposing(size, range)
import BlockModel
import BlockModelDirection


coordinates: List BlockModel.XYZRow
coordinates =  lift3 (\x y z -> {x=x,y=y,z=z}) range range range


mapSingleBlock: BlockModel.XYZRow ->  BlockModel.XYZColorRow
mapSingleBlock xyz =
    { rows = xyz
    , colors = BlockModelDirection.xyzRowToXYZColor xyz }
    --, directions = BlockModelDirection.xyzRowToXYZDirection xyz }

allBlocks: List BlockModel.XYZColorRow
allBlocks = List.map mapSingleBlock coordinates

