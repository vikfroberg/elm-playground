module View exposing (view)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Model exposing (..)
import Msg exposing (..)
import ProductListPage
import ProductViewPage
import Repo exposing (Repo)


view : Model -> Html Msg
view model =
    let
        content =
            case model.pageState of
                ProductListPageState state ->
                    ProductListPage.view
                        { getProducts = Repo.getMany model.products }
                        state
                        |> Html.map ProductListPageMsg

                ProductViewPageState state ->
                    ProductViewPage.view
                        { getProduct = Repo.getOne model.products }
                        state
                        |> Html.map ProductViewPageMsg
    in
    div []
        [ text <| "Items in cart: " ++ String.fromInt (List.length model.cart)
        , content
        ]
