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
    , displayEdit : Bool
    }



---- MODEL ----


type alias Model =
    { todosList : List Todo
    , inputField : String
    , currentId : Int
    , newTodoName : String
    }


init : Model
init =
    { todosList = []
    , inputField = ""
    , currentId = 0
    , newTodoName = ""
    }



---- UPDATE ----


type Msg
    = NoOp
    | UpdateInput String
    | UpdateNewName String
    | EnterTodo
    | DeleteTodo Int
    | EditTodo Int
    | UpdateTodo Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateInput inputString ->
            ( { model | inputField = inputString }, Cmd.none )

        UpdateNewName inputString ->
            ( { model | newTodoName = inputString }, Cmd.none )

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

        UpdateTodo todoId newName ->
            ( { model | todosList = updateTodoName model todoId newName, newTodoName = "" }, Cmd.none )

        EditTodo todoId ->
            ( { model | todosList = showEditTodo model.todosList todoId }, Cmd.none )


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
    [ { id = model.currentId, name = model.inputField, completed = False, displayEdit = False } ]


deleteTodo : Model -> Int -> List Todo
deleteTodo model todoId =
    List.filter (\todo -> todo.id /= todoId) model.todosList


updateTodoName : Model -> Int -> String -> List Todo
updateTodoName model todoId newName =
    List.map
        (\t ->
            if t.id == todoId then
                { t
                    | name = newName
                    , displayEdit = False
                }

            else
                { t
                    | displayEdit = False
                }
        )
        model.todosList


showEditTodo : List Todo -> Int -> List Todo
showEditTodo todos todoId =
    List.map
        (\t ->
            if t.id == todoId then
                { t | displayEdit = not t.displayEdit }

            else
                t
        )
        (List.map
            (\t ->
                if t.id == todoId then
                    t

                else
                    { t | displayEdit = False }
            )
            todos
        )



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
                    , renderAllTodos model model.todosList
                    ]
                ]
            ]
        , div [ class "grid-sidebar" ] []
        ]


renderAllTodos : Model -> List Todo -> Html Msg
renderAllTodos model todos =
    Keyed.node "div" [ class "todo-wrapper" ] (List.map (renderKeyedTodo model) todos)


renderKeyedTodo : Model -> Todo -> ( String, Html Msg )
renderKeyedTodo model todo =
    ( todo.name, Lazy.lazy2 renderTodo model todo )


renderTodo : Model -> Todo -> Html Msg
renderTodo model todo =
    div [ class "todo-container" ]
        [ div [ class "todo" ]
            [ text todo.name
            , div [ class "todo-icons" ]
                [ div [ class "todo-icon" ]
                    [ img
                        [ src "edit.svg"
                        , onClick (EditTodo todo.id)
                        ]
                        []
                    ]
                , div [ class "todo-icon" ]
                    [ img
                        [ src "trash-2.svg"
                        , onClick (DeleteTodo todo.id)
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "todo__update-name", classList [ ( "hidden", not todo.displayEdit ) ] ]
            [ input
                [ type_ "text"
                , placeholder "Enter new Todo name"
                , value model.newTodoName
                , onInput UpdateNewName
                , onEnter (UpdateTodo todo.id model.newTodoName)
                ]
                []
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
