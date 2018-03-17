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
    , hour : Int
    , activities : List Activity
    }


type alias Filter =
    { show : Bool
    , timeFrom : Int
    , timeTo : Int
    , minTimeSpent : Int
    , categories : List String
    , query : Maybe String
    }


type alias Model =
    { data : List Hour
    , token : Maybe Token
    , page : Page
    , filter : Filter
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
    | ResetFilter
    | FilterSet String String
