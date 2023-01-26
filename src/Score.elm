module Score exposing (Highscore, highscoreDecoder)

import Json.Decode as Decode exposing (Decoder)


type alias Highscore =
    { player : String, score : Int }


highscoreDecoder : Decoder (List Highscore)
highscoreDecoder =
    Decode.list (Decode.map2 Highscore (Decode.field "name" Decode.string) (Decode.field "score" Decode.int))
