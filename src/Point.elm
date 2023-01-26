module Point exposing (Point, generator, move)

import Area
import Direction
import Random



-- Point and Point Functions


type alias Point =
    { x : Int, y : Int }


move : Direction.Direction -> Point -> Point
move direction point =
    case direction of
        Direction.Up ->
            { point | y = point.y - 1 }

        Direction.Down ->
            { point | y = point.y + 1 }

        Direction.Left ->
            { point | x = point.x - 1 }

        Direction.Right ->
            { point | x = point.x + 1 }


generator : Area.Area -> Random.Generator Point
generator area =
    Random.map2 (\px py -> { x = px, y = py }) (Random.int 0 (area.width - 1)) (Random.int 0 (area.height - 1))
