module Hello exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import List


type alias Model =
    { todos : List String
    , inputFieldState : InputFieldModel
    }


type alias InputFieldModel =
    { text : String
    , buttonActive : Bool
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
    | NoOp


type InputFieldUpdate
    = ChangeText String
    | AddTodo


view : Model -> Html Msg
view model =
    div []
        ((renderList model.todos) ++ [ renderInput ])


renderList : List String -> List (Html msg)
renderList texts =
    List.map (\h -> text h) texts


renderInput : Html Msg
renderInput =
    div []
        [ input [ onInput (FieldUpdate ChangeText) ] []
        , button [ onClick AddTodo ] [ text "Add" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FieldUpdate inputFieldUpdate ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateInputState event model =
    model


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
