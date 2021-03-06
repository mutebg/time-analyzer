module Data exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipe
import Json.Encode as Encode
import Http
import Types exposing (..)
import Date exposing (..)


apiBase : String
apiBase =
    --"http://localhost:5000/etsy-syncer/us-central1/api/"
    "https://us-central1-time-analyzer-68cd5.cloudfunctions.net/api/"


productivityList : List ProductivityType
productivityList =
    [ { index = 2, label = "Very productive" }
    , { index = 1, label = "Productive" }
    , { index = 0, label = "Neutral" }
    , { index = -1, label = "Very distractin" }
    , { index = -2, label = "Very distracting" }
    ]


documentDecoder : Decode.Decoder Document
documentDecoder =
    DecodePipe.decode Document
        |> DecodePipe.required "TimeSpent" Decode.int
        |> DecodePipe.required "Name" Decode.string


activityDecoder : Decode.Decoder Activity
activityDecoder =
    DecodePipe.decode Activity
        |> DecodePipe.required "TotalTimeSpent" Decode.int
        |> DecodePipe.required "Activity" Decode.string
        |> DecodePipe.required "Documents" (Decode.list documentDecoder)
        |> DecodePipe.required "Category" Decode.string
        |> DecodePipe.required "Productivity" Decode.int


hourDecoder : Decode.Decoder Hour
hourDecoder =
    DecodePipe.decode Hour
        |> DecodePipe.required "Date" Decode.string
        |> DecodePipe.required "Hour" Decode.int
        |> DecodePipe.required "Activities" (Decode.list activityDecoder)


dataDecoder : Decode.Decoder (List Hour)
dataDecoder =
    Decode.list hourDecoder


loadDocuments : String -> String -> String -> Cmd Msg
loadDocuments fromDate toDate token =
    let
        url =
            apiBase ++ "document/" ++ toDate ++ "/" ++ fromDate ++ "/?token=" ++ token

        request =
            Http.get url dataDecoder
    in
        Http.send LoadDocuments request
