module Hello exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import List
import String


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
    div []
        ((renderList model.todos) ++ [ renderInput ])


renderList : List Todo -> List (Html msg)
renderList todos =
    List.map (\h -> text h.text) todos


renderInput : Html Msg
renderInput =
    div []
        [ input [ onInput (FieldUpdate << ChangeText) ] []
        , button [ onClick (AddTodo) ] [ text "Add" ]
        ]


constructChangeText : String -> Msg
constructChangeText text =
    FieldUpdate (ChangeText text)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FieldUpdate inputFieldUpdate ->
            ( { model | inputFieldState = (handleFieldUpdate inputFieldUpdate model.inputFieldState) }, Cmd.none )

        AddTodo ->
            ( { model | inputFieldState = clearInputText model.inputFieldState, todos = model.todos ++ [ (createTodo model.inputFieldState) ] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


createTodo : InputFieldModel -> Todo
createTodo model =
    { text = model.text }


clearInputText : InputFieldModel -> InputFieldModel
clearInputText model =
    { model | text = "" }


handleFieldUpdate : InputFieldUpdate -> InputFieldModel -> InputFieldModel
handleFieldUpdate msg model =
    case msg of
        ChangeText newText ->
            { model | text = newText, buttonActive = String.isEmpty newText }


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
