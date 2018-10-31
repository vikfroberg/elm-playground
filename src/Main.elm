module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    ()


type Msg
    = Noop


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [] [ text "Hello world!" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
