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
import List.Extra exposing (unique)


dailyView : Model -> String -> Html Msg
dailyView model day =
    div []
        [ printHeader day
        , printDay model.data model.filter
        ]


printHeader : String -> Html Msg
printHeader day =
    div [ class "header" ]
        [ div [ class "header__filter" ]
            [ button [ onClick ToggleFilter, class "btn" ] [ text "Filter" ]
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


printDay : List Hour -> Filter -> Html Msg
printDay hours filter =
    let
        className =
            if filter.show then
                " main--open"
            else
                ""
    in
        div [ class <| "main" ++ className ]
            [ div [ class "filter" ] [ printFilter filter hours ]
            , div [ class "day" ]
                (hours
                    |> List.filter (filterHour filter)
                    |> List.map (printHour filter)
                )
            ]


printHour : Filter -> Hour -> Html Msg
printHour filter h =
    div [ class "hour" ]
        [ h2 [ class "hour__title" ] [ text <| toString h.hour ]
        , div [ class "hour__list" ]
            (h.activities
                |> List.filter (filterActiviry filter)
                |> List.map printActivity
            )
        ]


printActivity : Activity -> Html Msg
printActivity a =
    div [ class "activity" ]
        [ span [ class "activity__time" ] [ text <| secToMin a.totalTimeSpent ]
        , span [ class "activity__name" ] [ text a.activity ]
        , div [ class "activity__popup" ]
            [ printActivityPopup a
            ]
        ]


printActivityPopup : Activity -> Html Msg
printActivityPopup a =
    let
        docs =
            a.documents
                |> List.map
                    (\c ->
                        dd []
                            [ span [ class "activity-popup__time" ] [ text <| secToMin c.timeSpent ]
                            , span [ title c.name ] [ text c.name ]
                            ]
                    )
    in
        div [ class "activity-popup" ]
            [ dl []
                ([ dt [] [ text "Name" ]
                 , dd [] [ text a.activity ]
                 , dt [] [ text "Category" ]
                 , dd [] [ text a.category ]
                 , dt [] [ text "Productivity" ]
                 , dd []
                    [ text <| formatProductivity a.productivity ]
                 , dt
                    []
                    [ text "Total time" ]
                 , dd [] [ text <| secToMin a.totalTimeSpent ]
                 , dt [] [ text "Documents" ]
                 ]
                    ++ docs
                )
            ]


filterHour : Filter -> Hour -> Bool
filterHour filter hour =
    if hour.hour >= filter.timeFrom && hour.hour <= filter.timeTo then
        True
    else
        False


filterActiviry : Filter -> Activity -> Bool
filterActiviry filter act =
    let
        minTime =
            filter.minTimeSpent * 60

        docs =
            List.filter (filterDocument filter) act.documents

        filterCategory =
            if List.length filter.categories == 0 then
                True
            else
                not (List.member act.category filter.categories)

        matchActivityName =
            case filter.query of
                Just term ->
                    String.contains term act.activity

                _ ->
                    True
    in
        if act.totalTimeSpent >= minTime && filterCategory && (List.length docs > 0 || matchActivityName) then
            True
        else
            False


filterDocument : Filter -> Document -> Bool
filterDocument filter doc =
    case filter.query of
        Just term ->
            String.contains term doc.name

        _ ->
            True


printFilter : Filter -> List Hour -> Html Msg
printFilter f h =
    let
        uniqCat =
            getAllUniqCategory h

        queryStr =
            Maybe.withDefault "" f.query
    in
        div []
            [ h2 [ class "filter__title" ]
                [ text "Time intefval" ]
            , div [ class "filter__section time-interval" ]
                [ div
                    []
                    [ label [ for "time-from", class "help" ] [ text "From" ]
                    , input
                        [ type_ "number"
                        , id "time-from"
                        , onInput (FilterSet "timeFrom")
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "23"
                        , value <| toString f.timeFrom
                        ]
                        []
                    ]
                , div []
                    [ label [ for "time-to", class "help" ] [ text "To" ]
                    , input
                        [ type_ "number"
                        , id "time-to"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "23"
                        , onInput (FilterSet "timeTo")
                        , value <| toString f.timeTo
                        ]
                        []
                    ]
                ]
            , div [ class "filter__section" ]
                [ label
                    [ for "min-spent-time", class "filter__title" ]
                    [ text "Min spent time" ]
                , input
                    [ type_ "number"
                    , id "min-spent-time"
                    , Html.Attributes.min "0"
                    , onInput (FilterSet "minTimeSpent")
                    , value <| toString f.minTimeSpent
                    ]
                    []
                ]
            , div [ class "filter__section" ]
                [ label [ for "query", class "filter__title" ] [ text "Document name" ]
                , input
                    [ type_ "search"
                    , id "query"
                    , onInput (FilterSet "query")
                    , value <| queryStr
                    ]
                    []
                ]
            , div [ class "filter__section" ]
                [ h2 [ class "filter__title" ]
                    [ text "Categories" ]
                , printFilterCategories uniqCat f.categories
                ]
            , div
                []
                [ button [ onClick ResetFilter, class "btn" ] [ text "reset filter" ]
                ]
            ]


printFilterCategories : List String -> List String -> Html Msg
printFilterCategories allCats selected =
    ul []
        (allCats
            |> List.map
                (\c ->
                    li
                        [ onClick (FilterSet "category" c)
                        , class
                            (if List.member c selected then
                                "selected"
                             else
                                ""
                            )
                        ]
                        [ text c ]
                )
        )


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


formatProductivity : Int -> String
formatProductivity p =
    toString p


getAllUniqCategory : List Hour -> List String
getAllUniqCategory hours =
    hours
        |> List.map (\h -> h.activities)
        |> List.concat
        |> List.map (\a -> a.category)
        |> unique
        |> List.sort


loginView : String -> Html Msg
loginView token =
    div [ class "login-page" ]
        [ Html.form [ onSubmit SubmitLoginForm ]
            [ p []
                [ text "In order to access this page you need API key from www.rescuetime.com, you can find your key "
                , a [ href "https://www.rescuetime.com/anapi/manage", target "blank" ] [ text "here" ]
                ]
            , label [ for "token" ] [ text "Enter your token" ]
            , input [ id "token", type_ "text", onInput (\e -> TypeToken e) ] []
            , button [ class "btn" ] [ text "login" ]
            ]
        ]
