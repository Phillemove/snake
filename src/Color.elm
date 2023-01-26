module Color exposing (Color(..), toString)

-- Color and Color toString Function


type Color
    = Lightgrey
    | Blue
    | HSL Int Int Int


toString : Color -> String
toString color =
    case color of
        Lightgrey ->
            "lightgrey"

        Blue ->
            "blue"

        HSL shade saturation brightness ->
            "hsl(" ++ String.fromInt shade ++ ", " ++ String.fromInt saturation ++ "%, " ++ String.fromInt brightness ++ "%)"
