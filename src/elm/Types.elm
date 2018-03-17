module Types exposing (..)

import Date exposing (..)
import Http
import Navigation


type alias Token =
    String


type alias Document =
    { timeSpent : Int
    , name : String
    }


type alias Activity =
    { totalTimeSpent : Int
    , activity : String
    , documents : List Document
    , category : String
    , productivity : Int
    }


type alias Hour =
    { date : String
    , activities : List Activity
    }


type alias Model =
    { data : List Hour
    , token : Maybe Token
    , page : Page
    }


type Page
    = Login
    | Daily String


type Msg
    = NoOp
    | UrlChange Navigation.Location
    | LoadDocuments (Result Http.Error (List Hour))
    | RequestToday
    | ReceiveToday Date
    | TypeToken String
    | SubmitLoginForm
    | ReceiveToken String
    | Logout
    | ToggleFilter
