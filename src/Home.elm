module Home exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onInput, onClick)

main = 
    Browser.sandbox { init = init, update = update, view = view }

type alias Todo =
    { text: String
    , completed: Bool
    }

type alias Model =
    { todos: List Todo
    , inputText: String
    }

init: Model
init = 
    { todos = []
    , inputText = ""
    }
    

type Msg = 
    AddTask
    | RemoveTask Int
    | ToggleTask Int
    | ChangeInput String

addToList: String -> List Todo -> List Todo
addToList input todos =
    todos ++ [{ text = input, completed = False }]

removeFromList: Int -> List Todo -> List Todo
removeFromList index list =
    List.take index list ++ List.drop (index + 1) list

toggleAtIndex: Int -> List Todo -> List Todo
toggleAtIndex indexToToggle list =
    List.indexedMap (\currentIndex todo ->
        if currentIndex == indexToToggle then
            { todo | completed = not todo.completed }
        else
            todo
    ) list

viewTodo: Int -> Todo -> Html Msg
viewTodo index todo =
    if todo.text /= "" then
        li
        [ style "text-decoration"
            (if todo.completed then
                "line-through"
             else
                "none"
            )
        ]
        [ text todo.text 
        , button [ style "margin" "0 8px", style "border-radius" "4px", type_ "button", onClick (ToggleTask index) ] [ 
            if todo.completed == False then text "Finish task" else text "Back to task"
         ]
        , button [ style "border-radius" "4px", type_ "button", onClick (RemoveTask index) ] [ text "Delete" ]
        ]
    else text ""

update: Msg -> Model -> Model
update msg model =
    case msg of
        AddTask ->
            { model
                | todos = addToList model.inputText model.todos
                , inputText = ""
            }
        
        RemoveTask index ->
            { model | todos = removeFromList index model.todos }
        
        ToggleTask index ->
            { model | todos = toggleAtIndex index model.todos }

        ChangeInput input ->
            { model | inputText = input }

formAttributes: List (Attribute Msg) 
formAttributes =
    [ onSubmit AddTask
    , style "width" "100%"
    , style "height" "100vh"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    , style "background-color" "cyan"
    ]

divStyle: List (Attribute msg)
divStyle =
    [ style "width" "100%"
    , style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "margin-top" "5px"
    ]

buttonAttributes: List (Attribute Msg)
buttonAttributes =
    [ onClick AddTask
    , style "border-radius" "4px"
    , style "height" "35px"
    , style "margin-left" "4px"
    ]

view: Model -> Html Msg
view model =
    Html.form formAttributes
        [ h1 [] [ text "To-do app with Elm" ]
        , div []
        [input [ value model.inputText, onInput ChangeInput, placeholder "Add a new task", style "border-radius" "4px", style "padding" "8px", style "outline" "none" ] []
        , button buttonAttributes [ text "Add Task" ]
        ]
        , div divStyle
        [ol [] (List.indexedMap viewTodo model.todos)]
        
        ]