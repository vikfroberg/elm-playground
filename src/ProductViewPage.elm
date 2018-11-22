module ProductViewPage exposing (Msg, OutMsg(..), State, init, update, view)

import Html exposing (Html, button, div, h3, text)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..))
import Http


type alias State =
    { id : RemoteData String String
    }


type Msg
    = PressedAddToCart String
    | ReceviedProduct (Result Http.Error String)


type OutMsg
    = LoadProduct String (Result Http.Error String -> Msg)
    | AddToCart String
    | Noop


init : String -> ( State, OutMsg )
init id =
    ( { id = RemoteData.Loading }
    , LoadProduct id ReceviedProduct
    )


update : Msg -> State -> ( State, OutMsg )
update msg state =
    case msg of
        ReceviedProduct result ->
            case result of
                Ok id ->
                    ( { state | id = RemoteData.Success id }, Noop )
                Err err ->
                    ( state, Noop )

        PressedAddToCart id ->
            ( state, AddToCart id )


view :
    { getProduct : String -> Maybe (Product a) }
    -> State
    -> Html Msg
view { getProduct } model =
    let
        remoteProduct =
            model.id
                |> RemoteData.map getProduct
    in
    case remoteProduct of
        Success maybeProduct ->
            case maybeProduct of
                Just product ->
                    div [] [ viewProduct product ]

                Nothing ->
                    div [] [ text "Error while loading product" ]

        Failure err ->
            div [] [ text "Error while loading product" ]

        Loading ->
            div [] [ text "Loading..." ]

        NotAsked ->
            div [] [ text "Loading..." ]


type alias Product a =
    { a
        | id : String
        , name : String
    }


viewProduct : Product a -> Html Msg
viewProduct product =
    div []
        [ h3 [] [ text product.name ]
        , button [ onClick (PressedAddToCart product.id) ] [ text "Add to cart" ]
        ]
