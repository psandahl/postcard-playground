module Main exposing (main)

import Html exposing (Html)


type alias Model =
    { dummy : Int
    }


type Msg
    = Nop


init : ( Model, Cmd Msg )
init =
    ( { dummy = 1 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.text "Hello"


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
