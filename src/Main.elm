module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Json.Decode as Json



---- TYPES ----


type alias Todo =
    { name : String
    , completed : Bool
    }



---- MODEL ----


type alias Model =
    { todosList : List Todo
    , inputField : String
    }


init : Model
init =
    { todosList = []
    , inputField = ""
    }



---- UPDATE ----


type Msg
    = NoOp
    | UpdateInput String
    | EnterTodo
    | DeleteTodo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateInput inputString ->
            ( { model | inputField = inputString }, Cmd.none )

        EnterTodo ->
            ( if String.isEmpty model.inputField then
                model

              else
                { model
                    | todosList = List.append model.todosList (createTodo model)
                    , inputField = ""
                }
            , Cmd.none
            )

        DeleteTodo ->
            ( model, Cmd.none )


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        enterKey code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not enter key"
    in
    on "keydown" (Json.andThen enterKey keyCode)


createTodo : Model -> List Todo
createTodo model =
    [ { name = model.inputField, completed = False } ]



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
                        [ text "Enter a Todo below"
                        , div [ class "todos-input__wrapper" ]
                            [ input
                                [ type_ "text"
                                , placeholder "Clean the car"
                                , value model.inputField
                                , onInput UpdateInput
                                , onEnter EnterTodo
                                ]
                                []
                            ]
                        ]
                    , renderAllTodos model.todosList
                    ]
                ]
            ]
        , div [ class "grid-sidebar" ] [ text "sidebar" ]
        ]


renderAllTodos : List Todo -> Html Msg
renderAllTodos todos =
    Keyed.node "div" [ class "todo-wrapper" ] (List.map renderKeyedTodo todos)


renderKeyedTodo : Todo -> ( String, Html Msg )
renderKeyedTodo todo =
    ( todo.name, Lazy.lazy renderTodo todo )


renderTodo : Todo -> Html Msg
renderTodo todo =
    div [ class "todo" ] [ text todo.name ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> ( init, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        }
