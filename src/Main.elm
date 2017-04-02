module Main exposing (..)

import Html exposing (beginnerProgram, span, Html, ul, li, p, text, div, button, input, h1)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (map, append, filter, length)
import Debug


-- MODEL


type alias Todo =
    { id : Int
    , todo : String
    , done : Bool
    }


type alias Model =
    { todos : List Todo
    , tempTodo : String
    }


initModel : Model
initModel =
    { todos = []
    , tempTodo = ""
    }



-- UPDATE


type Msg
    = Add Model
    | Change String
    | Remove Int
    | ToggleTodo Int


adding : Model -> Model
adding model =
    let
        toda =
            Todo (length model.todos) model.tempTodo False

        newTodos =
            append model.todos [ toda ]
    in
        { model
            | todos = newTodos
            , tempTodo = ""
        }


utilFlipBool : Bool -> Bool
utilFlipBool b =
    if b == True then
        False
    else
        True


buttonText : Todo -> String
buttonText a =
    case a.done of
        True ->
            "undo"

        False ->
            "done"


buttonColorClass : Todo -> String
buttonColorClass a =
    case a.done of
        True ->
            "btn btn-warning-style"

        False ->
            "btn btn-sucess-style"


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add model ->
            adding model

        Change newTodo ->
            { model
                | tempTodo = newTodo
            }

        Remove id ->
            { model
                | todos = filter (\todo -> todo.id /= id) model.todos
            }

        ToggleTodo id ->
            let
                newTodos =
                    List.map
                        (\todo ->
                            if todo.id == id then
                                { todo
                                    | done = (utilFlipBool todo.done)
                                }
                            else
                                todo
                        )
                        model.todos
            in
                Debug.log "model"
                    { model
                        | todos = newTodos
                    }



-- VIEW


viewTodos : Model -> Html Msg
viewTodos model =
    ul [ class "list-group" ]
        (List.map viewOneTodo model.todos)


viewOneTodo : Todo -> Html Msg
viewOneTodo a =
    let
        color =
            if a.done == True then
                "green"
            else
                ""
    in
        li [ (class "list-group-item lead"), style [ ( "color", color ) ] ]
            [ text a.todo
            , button [ class (buttonColorClass a), onClick (ToggleTodo a.id) ] [ text (buttonText a) ]
            , button [ (class "btn btn-danger-style"), onClick (Remove a.id) ] [ text "remove" ]
            ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "raw" ]
            [ h1 [] [ text "my todos" ]
            , input [ placeholder "add your todo", onInput Change, value model.tempTodo ] []
            , button [ (class "btn btn-primary-style"), onClick (Add model) ] [ text "add" ]
            , viewTodos model
            ]
        ]


main : Program Never Model Msg
main =
    beginnerProgram { model = initModel, view = view, update = update }
