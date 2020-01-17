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
    { id : Int
    , name : String
    , completed : Bool
    }



---- MODEL ----


type alias Model =
    { todosList : List Todo
    , inputField : String
    , currentId : Int
    }


init : Model
init =
    { todosList = []
    , inputField = ""
    , currentId = 0
    }



---- UPDATE ----


type Msg
    = NoOp
    | UpdateInput String
    | EnterTodo
    | DeleteTodo Int


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
                    | currentId = model.currentId + 1
                    , todosList = List.append model.todosList (createTodo model)
                    , inputField = ""
                }
            , Cmd.none
            )

        DeleteTodo todoId ->
            ( { model | todosList = deleteTodo model todoId }, Cmd.none )


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
    [ { id = model.currentId, name = model.inputField, completed = False } ]


deleteTodo : Model -> Int -> List Todo
deleteTodo model todoId =
    List.filter (\todo -> todo.id /= todoId) model.todosList



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
        , div [ class "grid-sidebar" ] []
        ]


renderAllTodos : List Todo -> Html Msg
renderAllTodos todos =
    Keyed.node "div" [ class "todo-wrapper" ] (List.map renderKeyedTodo todos)


renderKeyedTodo : Todo -> ( String, Html Msg )
renderKeyedTodo todo =
    ( todo.name, Lazy.lazy renderTodo todo )


renderTodo : Todo -> Html Msg
renderTodo todo =
    div [ class "todo" ]
        [ text todo.name
        , div [ class "todo-icons" ]
            [ div [ class "todo-icon" ]
                [ img [ src "edit.svg" ] [] ]
            , div [ class "todo-icon" ]
                [ img
                    [ src "trash-2.svg"
                    , onClick (DeleteTodo todo.id)
                    ]
                    []
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> ( init, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        }
