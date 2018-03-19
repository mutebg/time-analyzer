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
import List.Extra exposing (remove)


-- APP


main : Program Never Model Msg
main =
    Navigation.program UrlChange { init = init, view = view, update = update, subscriptions = subscriptions }


defaultFilter : Filter
defaultFilter =
    { show = False
    , timeFrom = 0
    , timeTo = 23
    , minTimeSpent = 0
    , categories = []
    , query = Nothing
    , productivity = []
    , openActivity = Nothing
    }


model : Model
model =
    { data = []
    , token = Nothing
    , page = Login
    , filter = defaultFilter
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
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

        NavigateToDay date ->
            ( model, Navigation.newUrl ("#day/" ++ date) )

        TypeToken token ->
            ( { model | token = Just token }, Cmd.none )

        SubmitLoginForm ->
            ( model, sendToken <| Maybe.withDefault "" model.token )

        ReceiveToken token ->
            let
                msg =
                    case model.page of
                        Login ->
                            Cmd.batch [ requestToday ]

                        Daily x ->
                            Cmd.none
            in
                ( { model | token = Just token }, msg )

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

        ToggleFilter ->
            let
                currFilter =
                    model.filter

                filter =
                    { currFilter | show = not currFilter.show }
            in
                ( { model | filter = filter }, Cmd.none )

        FilterSet key value ->
            let
                newFilter =
                    updateFilter model.filter key value
            in
                ( { model | filter = newFilter }, Cmd.none )

        ResetFilter ->
            ( { model | filter = defaultFilter }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateFilter : Filter -> String -> String -> Filter
updateFilter f key value =
    let
        intValue =
            Result.withDefault 0 (String.toInt value)
    in
        case key of
            "query" ->
                let
                    query =
                        if value == "" then
                            Nothing
                        else
                            Just value
                in
                    { f | query = query }

            "openActivity" ->
                let
                    id =
                        if value == "" || Just value == f.openActivity then
                            Nothing
                        else
                            Just value
                in
                    { f | openActivity = id }

            "timeFrom" ->
                { f | timeFrom = intValue }

            "timeTo" ->
                { f | timeTo = intValue }

            "minTimeSpent" ->
                { f | minTimeSpent = intValue }

            "category" ->
                let
                    newCategories =
                        if List.member value f.categories then
                            remove value f.categories
                        else
                            value :: f.categories
                in
                    { f | categories = newCategories }

            "productivity" ->
                let
                    newCategories =
                        if List.member intValue f.productivity then
                            remove intValue f.productivity
                        else
                            intValue :: f.productivity
                in
                    { f | productivity = newCategories }

            _ ->
                f



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
