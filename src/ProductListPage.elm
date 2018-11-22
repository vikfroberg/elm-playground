module ProductListPage exposing (init, view, update, OutMsg(..), State, Msg)

import Html exposing (h3, div, button, text, Html)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..))
import Http


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
    | Noop


init : ( State, OutMsg )
init =
    ( { productIds = RemoteData.NotAsked }
    , LoadProducts ReceviedProducts
    )


update : Msg -> State -> ( State, OutMsg )
update msg state =
    case msg of
        ReceviedProducts result ->
            case result of
                Ok ids ->
                    ( { state | productIds = RemoteData.Success ids }, Noop )
                Err err ->
                    ( state, Noop )

        PressedAddToCart id ->
            ( state, AddToCart id )

        PressedTitle id ->
            ( state, GoProduct id )


view :
    { getProducts : (List String -> List (Product a)) }
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
