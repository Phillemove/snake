module Snake exposing (Snake, extend, hasCollision, hitFood, isOutOfBounds, move)

import Area
import Direction
import List.Extra
import List.NonEmpty exposing (NonEmpty(..))
import Point exposing (Point)



-- Snake and Snake Functions


type alias Snake =
    NonEmpty Point


move : Direction.Direction -> Snake -> Snake
move direction snake =
    Head (Point.move direction (List.NonEmpty.head snake)) (List.NonEmpty.removeLast snake)


extend : Snake -> Snake
extend snake =
    List.NonEmpty.snoc snake (List.NonEmpty.last snake)


isOutOfBounds : Snake -> Area.Area -> Bool
isOutOfBounds (Head point _) area =
    point.x >= area.width || point.x <= -1 || point.y >= area.height || point.y <= -1


hasCollision : Snake -> Bool
hasCollision (Head head tail) =
    List.Extra.any ((==) head) tail


hitFood : Snake -> Maybe Point.Point -> Bool
hitFood (Head head _) point =
    case point of
        Just p ->
            head == p

        Nothing ->
            False
