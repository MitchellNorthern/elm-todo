module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed exposing (..)



---- TYPES ----


type alias Todo =
    { name : String
    , completed : Bool
    }



---- MODEL ----


type alias Model =
    List Todo


init : Model
init =
    []



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "grid-container" ]
        [ div [ class "grid-sidebar" ] []
        , div [ class "grid-body" ]
            [ h1 [ class "body-title" ] [ text "Todo App in Elm" ]
            , div [ class "body-content" ]
                [ div [ class "todos-container" ]
                    [ div [ class "todos-heading" ]
                        [ text ""
                        , div [ class "todos-input__wrapper" ]
                            [ input [ class "todos-input__input", placeholder "Clean the car" ]
                                []
                            ]
                        ]
                    , renderAllTodos model
                    ]
                ]
            ]
        , div [ class "grid-sidebar" ] [ text "sidebar" ]
        ]


renderTodo : Todo -> Html Msg
renderTodo todo =
    div [ class "todo" ]
        [ text todo.name
        ]


renderAllTodos : List Todo -> Html Msg
renderAllTodos todos =
    div [ class "todo-wrapper" ]
        (List.map renderTodo todos)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> ( init, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        }
