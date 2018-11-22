module ProductListPage exposing (Msg, OutMsg(..), State, init, update, view)

import Html exposing (Html, button, div, h3, text)
import Html.Events exposing (onClick)
import Http
import RemoteData exposing (RemoteData(..))


type alias State =
    { productIds : RemoteData String (List String)
    }


type Msg
    = PressedAddToCart String
    | PressedTitle String
    | ReceviedProducts (Result Http.Error (List String))


type OutMsg
    = LoadProducts (Result Http.Error (List String) -> Msg)
    | AddToCart String
    | GoProduct String


init : ( State, List OutMsg )
init =
    ( { productIds = RemoteData.NotAsked }
    , [ LoadProducts ReceviedProducts ]
    )


update : Msg -> State -> ( State, List OutMsg )
update msg state =
    case msg of
        ReceviedProducts result ->
            case result of
                Ok ids ->
                    ( { state | productIds = RemoteData.Success ids }, [] )

                Err err ->
                    ( state, [] )

        PressedAddToCart id ->
            ( state, [ AddToCart id ] )

        PressedTitle id ->
            ( state, [ GoProduct id ] )


view :
    { getProducts : List String -> List (Product a) }
    -> State
    -> Html Msg
view { getProducts } model =
    let
        remoteProducts =
            model.productIds
                |> RemoteData.map getProducts
    in
    case remoteProducts of
        Success products ->
            if List.isEmpty products then
                div [] [ text "No products" ]

            else
                div [] (List.map viewProduct products)

        Failure err ->
            div [] [ text "Error while loading products" ]

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
        [ h3 [ onClick (PressedTitle product.id) ] [ text product.name ]
        , button [ onClick (PressedAddToCart product.id) ] [ text "Add to cart" ]
        ]
