module ProductViewPage exposing (Msg, OutMsg(..), State, init, update, view)

import Html exposing (Html, button, div, h3, text)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..))


type alias State =
    { id : RemoteData String Int
    }


type Msg
    = PressedAddToCart Int
    | ReceviedProduct Int


type OutMsg
    = LoadProducts Int (Int -> Msg)
    | AddToCart Int
    | Noop


init : Int -> ( State, OutMsg )
init id =
    ( { id = RemoteData.Loading }
    , LoadProducts id ReceviedProduct
    )


update : Msg -> State -> ( State, OutMsg )
update msg state =
    case msg of
        ReceviedProduct id ->
            ( { state | id = RemoteData.Success id }, Noop )

        PressedAddToCart id ->
            ( state, AddToCart id )


view :
    { getProduct : Int -> Product a }
    -> State
    -> Html Msg
view { getProduct } model =
    let
        remoteProduct =
            model.id
                |> RemoteData.map getProduct
    in
    case remoteProduct of
        Success product ->
            div [] [ viewProduct product ]

        Failure err ->
            div [] [ text "Error while loading product" ]

        Loading ->
            div [] [ text "Loading..." ]

        NotAsked ->
            div [] [ text "Loading..." ]


type alias Product a =
    { a
        | id : Int
        , name : String
    }


viewProduct : Product a -> Html Msg
viewProduct product =
    div []
        [ h3 [] [ text product.name ]
        , button [ onClick (PressedAddToCart product.id) ] [ text "Add to cart" ]
        ]
