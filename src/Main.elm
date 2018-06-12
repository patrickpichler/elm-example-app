module Hello exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List
import String
import Json.Decode as Json
import Html.Events exposing (..)


type alias Model =
    { todos : List Todo
    , inputFieldState : InputFieldModel
    }


type alias InputFieldModel =
    { text : String
    , buttonActive : Bool
    }


type alias Todo =
    { text : String
    }


init : ( Model, Cmd Msg )
init =
    ( { todos = []
      , inputFieldState = initInputFieldModel
      }
    , Cmd.none
    )


initInputFieldModel : InputFieldModel
initInputFieldModel =
    { text = ""
    , buttonActive = False
    }


type Msg
    = FieldUpdate InputFieldUpdate
    | AddTodo
    | NoOp


type InputFieldUpdate
    = ChangeText String


view : Model -> Html Msg
view model =
    case model of
        { todos, inputFieldState } ->
            div []
                ((renderList model.todos) ++ [ renderInput inputFieldState ])


renderList : List Todo -> List (Html msg)
renderList todos =
    List.map renderItem todos


renderItem : Todo -> Html msg
renderItem todo =
    div [ class "todo-item" ] [ text todo.text ]


renderInput : InputFieldModel -> Html Msg
renderInput model =
    div []
        [ input
            [ onInput (FieldUpdate << ChangeText)
            , onEnter AddTodo
            ]
            []
        , button
            [ onClick AddTodo
            , (not >> disabled) model.buttonActive
            ]
            [ text "Add" ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


constructChangeText : String -> Msg
constructChangeText text =
    FieldUpdate (ChangeText text)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FieldUpdate inputFieldUpdate ->
            ( { model
                | inputFieldState = (handleFieldUpdate inputFieldUpdate model.inputFieldState)
              }
            , Cmd.none
            )

        AddTodo ->
            ( { model
                | inputFieldState = clearInputText model.inputFieldState
                , todos = model.todos ++ [ (createTodo model.inputFieldState) ]
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


createTodo : InputFieldModel -> Todo
createTodo model =
    { text = model.text }


clearInputText : InputFieldModel -> InputFieldModel
clearInputText model =
    { model | text = "", buttonActive = False }


handleFieldUpdate : InputFieldUpdate -> InputFieldModel -> InputFieldModel
handleFieldUpdate msg model =
    case msg of
        ChangeText newText ->
            { text = newText, buttonActive = (not << String.isEmpty) newText }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
