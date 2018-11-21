module ProductListPage exposing (init, view, update, OutMsg(..), State, Msg)

import Html exposing (h3, div, button, text, Html)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..))


type alias State =
    { productIds : RemoteData String (List Int)
    }


type Msg
    = PressedAddToCart Int
    | ReceviedProducts (List Int)


type OutMsg
    = LoadProducts (List Int -> Msg)
    | AddToCart Int
    | Noop


init : ( State, OutMsg )
init =
    ( { productIds = RemoteData.NotAsked }
    , LoadProducts ReceviedProducts
    )


update : Msg -> State -> ( State, OutMsg )
update msg state =
    case msg of
        ReceviedProducts ids ->
            ( { state | productIds = RemoteData.Success ids }, Noop )
        PressedAddToCart id ->
            ( state, AddToCart id )


view :
    { getProducts : (List Int -> List (Product a)) }
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
        | id : Int
        , name : String
    }


viewProduct : Product a -> Html Msg
viewProduct product =
    div []
        [ h3 [] [ text product.name ]
        , button [ onClick (PressedAddToCart product.id) ] [ text "Add to cart" ]
        ]
