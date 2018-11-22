module View exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import ProductListPage
import ProductViewPage
import Model exposing (..)
import Msg exposing (..)
import Repo exposing (Repo)


view : Model -> Html Msg
view model =
    let
        content =
            case model.pageState of
                ProductListPageState state ->
                    ProductListPage.view
                        { getProducts = Repo.get model.products }
                        state
                        |> Html.map ProductListPageMsg

                ProductViewPageState state ->
                    ProductViewPage.view
                        { getProduct = List.singleton >> Repo.get model.products >> List.head }
                        state
                        |> Html.map ProductViewPageMsg
    in
    div []
        [ text <| "Items in cart: " ++ String.fromInt (List.length model.cart)
        , content
        ]


