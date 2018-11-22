module Main exposing (main)

import Browser
import Dict exposing (Dict)
import GraphQL
import GraphQLProduct
import Model exposing (..)
import Msg exposing (..)
import ProductListPage
import ProductViewPage
import Repo exposing (Repo)
import TupleExtra as Tuple
import View exposing (..)



-- TEA


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
    let
        toModel pageState =
            { products = Repo.empty .id
            , cart = []
            , pageState = pageState
            }
    in
    ProductListPage.init
        |> Tuple.mapFirst (ProductListPageState >> toModel)
        |> Tuple.foldlSecond productListPageUpdate
        |> Tuple.mapSecond Cmd.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    case msg of
        ProductRepoMsg subMsg ->
            Repo.update subMsg model.products
                |> Tuple.mapFirst (\s -> { model | products = s })
                |> Tuple.applyr update

        ProductListPageMsg subMsg ->
            case model.pageState of
                ProductListPageState state ->
                    ProductListPage.update subMsg state
                        |> Tuple.mapFirst (ProductListPageState >> setPageState model)
                        |> Tuple.foldlSecond productListPageUpdate
                        |> Tuple.mapSecond Cmd.batch

                _ ->
                    ( model, Cmd.none )

        ProductViewPageMsg subMsg ->
            case model.pageState of
                ProductViewPageState state ->
                    ProductViewPage.update subMsg state
                        |> Tuple.mapFirst (ProductViewPageState >> setPageState model)
                        |> Tuple.foldlSecond productViewPageUpdate
                        |> Tuple.mapSecond Cmd.batch

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Helpers


setPageState : Model -> PageState -> Model
setPageState model pageState =
    { model | pageState = pageState }



-- DI


productListPageUpdate : ProductListPage.OutMsg -> Model -> ( Model, Cmd Msg )
productListPageUpdate outMsg model =
    case outMsg of
        ProductListPage.GoProduct id ->
            ProductViewPage.init id
                |> Tuple.mapFirst (ProductViewPageState >> setPageState model)
                |> Tuple.foldlSecond productViewPageUpdate
                |> Tuple.mapSecond Cmd.batch

        ProductListPage.LoadProducts toMsg ->
            ( model
            , GraphQL.sendMock
                (Repo.InsertMany (toMsg >> ProductListPageMsg))
                (GraphQLProduct.decoderMany "products")
                (GraphQLProduct.encoder "products" GraphQLProduct.mock)
                |> Cmd.map ProductRepoMsg
            )

        ProductListPage.AddToCart id ->
            -- send to store later
            ( { model | cart = model.cart ++ [ id ] }
            , Cmd.none
            )


productViewPageUpdate : ProductViewPage.OutMsg -> Model -> ( Model, Cmd Msg )
productViewPageUpdate outMsg model =
    case outMsg of
        ProductViewPage.LoadProduct id toMsg ->
            case Repo.get model.products id of
                Just _ ->
                    update (ProductViewPageMsg (toMsg (Ok id))) model

                Nothing ->
                    ( model
                    , GraphQL.sendMock
                        (Repo.Insert (toMsg >> ProductViewPageMsg))
                        (GraphQLProduct.decoder "products")
                        (GraphQLProduct.encoder "products" GraphQLProduct.mock)
                        |> Cmd.map ProductRepoMsg
                    )

        ProductViewPage.AddToCart id ->
            -- send to store later
            ( { model | cart = model.cart ++ [ id ] }
            , Cmd.none
            )
