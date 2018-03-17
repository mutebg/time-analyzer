module Views exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck, onSubmit)
import Types exposing (..)
import Date.Extra.Period exposing (add, Period(..))
import Date.Extra.Utils exposing (unsafeFromString)
import Date exposing (..)
import Date.Extra.Config.Config_en_au exposing (config)
import Date.Extra.Format exposing (..)


dailyView : Model -> String -> Html Msg
dailyView model day =
    div []
        [ printHeader day
        , printDay model.data
        ]


printHeader : String -> Html Msg
printHeader day =
    div [ class "header" ]
        [ div [ class "header__filter" ]
            [ button [ onClick RequestToday, class "btn" ] [ text "Filter" ]
            ]
        , div
            [ class "header__date" ]
            [ a [ href <| "#day/" ++ (linkDay day -1), class "btn" ]
                [ text "<" ]
            , span []
                [ text day ]
            , a
                [ href <| "#day/" ++ (linkDay day 1), class "btn" ]
                [ text ">" ]
            , button [ onClick RequestToday, class "btn" ] [ text "Today" ]
            ]
        , div [ class "header__user" ]
            [ button [ onClick Logout, class "btn" ] [ text "Logout" ]
            ]
        ]


printDay : List Hour -> Html Msg
printDay hours =
    div [ class "main" ]
        [ div [ class "day" ]
            (List.map printHour hours)
        ]


printHour : Hour -> Html Msg
printHour h =
    div [ class "hour" ]
        [ h2 [ class "hour__title" ] [ text <| parseHour h.date ]
        , div [ class "hour__list" ] (List.map printActivity h.activities)
        ]


printActivity : Activity -> Html Msg
printActivity a =
    div [ class "activity" ]
        [ span [ class "activity__time" ] [ text <| secToMin a.totalTimeSpent ]
        , span [ class "activity__name" ] [ text a.activity ]
        , span [ class "activity__category" ] [ text a.category ]
        ]


secToMin : Int -> String
secToMin s =
    let
        secs =
            s % 60

        seconds =
            if secs > 9 then
                toString secs
            else
                "0" ++ (toString secs)

        minutes =
            toString <| floor <| (toFloat s) / 60
    in
        minutes ++ ":" ++ seconds


formatDate : Date -> String
formatDate date =
    format config "%Y-%m-%d" date


linkDay : String -> Int -> String
linkDay date days =
    date
        |> unsafeFromString
        |> add Day days
        |> formatDate


parseHour : String -> String
parseHour date =
    date
        |> unsafeFromString
        |> hour
        |> toString


loginView : String -> Html Msg
loginView token =
    div [ class "login-page" ]
        [ p []
            [ text "In order to access this page you need API key from Resque time, You can find your key "
            , a [ href "https://www.rescuetime.com/anapi/manage", target "blank" ] [ text "here" ]
            ]
        , Html.form [ onSubmit SubmitLoginForm ]
            [ label [ for "token" ] [ text "Enter your token" ]
            , input [ id "token", type_ "text", onInput (\e -> TypeToken e) ] []
            , button [] [ text "login" ]
            ]
        ]
