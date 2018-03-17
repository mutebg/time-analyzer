port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (..)
import Views exposing (..)
import Data exposing (..)
import Debug exposing (log)
import Date exposing (..)
import Task exposing (..)
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)


-- APP


main : Program Never Model Msg
main =
    Navigation.program UrlChange { init = init, view = view, update = update, subscriptions = subscriptions }


model : Model
model =
    { data = []
    , token = Nothing
    , page = Login
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    --( model, Cmd.batch [ (loadDocuments model.day model.day model.token) ] )
    ( model, Cmd.none )


routeParse : Url.Parser (Page -> a) a
routeParse =
    Url.oneOf
        [ Url.map Login top
        , Url.map Daily (Url.s "day" </> Url.string)
        ]



-- SUBSCRIBTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveToken ReceiveToken
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            let
                newPage =
                    case Url.parseHash routeParse location of
                        Just page ->
                            page

                        _ ->
                            Login

                newMsg =
                    case newPage of
                        Types.Daily day ->
                            Cmd.batch [ checkToken model.token (loadDocuments day day) ]

                        _ ->
                            Cmd.none
            in
                ( { model | page = newPage }, newMsg )

        RequestToday ->
            ( model, Cmd.batch [ requestToday ] )

        ReceiveToday date ->
            ( model, Navigation.newUrl ("#day/" ++ (formatDate date)) )

        TypeToken token ->
            ( { model | token = Just token }, Cmd.none )

        SubmitLoginForm ->
            ( model, sendToken <| Maybe.withDefault "" model.token )

        ReceiveToken token ->
            ( { model | token = Just token }, Cmd.batch [ requestToday ] )

        Logout ->
            ( { model | token = Nothing }, Cmd.batch [ sendToken "", Navigation.newUrl "#login" ] )

        LoadDocuments (Ok data) ->
            ( { model | data = data }, Cmd.none )

        LoadDocuments (Err error) ->
            let
                _ =
                    Debug.log "ERR " error
            in
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html Msg
view model =
    div []
        [ case model.page of
            Login ->
                loginView <| Maybe.withDefault "" model.token

            Daily day ->
                dailyView model day
        ]


requestToday : Cmd Msg
requestToday =
    Task.perform ReceiveToday Date.now


checkToken : Maybe String -> (Token -> Cmd msg) -> Cmd msg
checkToken token task =
    let
        login =
            Navigation.newUrl "#login"
    in
        case token of
            Nothing ->
                login

            Just "" ->
                login

            Just token ->
                task token



-- PORTS


port sendToken : Token -> Cmd msg


port receiveToken : (Token -> msg) -> Sub msg
