module Main exposing (main)

import Browser
import Dict exposing (Dict)
import GraphQL
import GraphQLProduct
import ProductListPage
import ProductViewPage
import Model exposing (..)
import Msg exposing (..)
import View exposing (..)
import Repo exposing (Repo)


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
        ( productListPageState, productListPageOutMsg ) =
            ProductListPage.init
    in
    productListPageToUpdate
        productListPageOutMsg
        ( { pageState = ProductListPageState productListPageState
          , products = Repo.empty
          , cart = []
          }
        , Cmd.none
        )


productListPageToUpdate : ProductListPage.OutMsg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
productListPageToUpdate outMsg ( model, cmd ) =
    case outMsg of
        ProductListPage.GoProduct id ->
            let
                ( productViewPageState, productViewPageOutMsg ) =
                    ProductViewPage.init id
            in
            productViewPageToUpdate
                productViewPageOutMsg
                ( { model | pageState = ProductViewPageState productViewPageState }
                , Cmd.none
                )

        ProductListPage.LoadProducts toMsg ->
            ( model
            , Cmd.batch
                [ GraphQL.sendMock
                    (Repo.InsertMany .id (toMsg >> ProductListPageMsg) >> ProductRepoMsg)
                    (GraphQLProduct.decoderMany "products")
                    (GraphQLProduct.encoder "products" GraphQLProduct.mock)
                , cmd
                ]
            )

        ProductListPage.AddToCart id ->
            -- send to store later
            ( { model | cart = model.cart ++ [ id ] }
            , cmd
            )

        ProductListPage.Noop ->
            ( model, cmd )


productViewPageToUpdate : ProductViewPage.OutMsg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
productViewPageToUpdate outMsg ( model, cmd ) =
    case outMsg of
        ProductViewPage.LoadProduct id toMsg ->
            ( model
            , Cmd.batch
                [ GraphQL.sendMock
                    (Repo.Insert .id (toMsg >> ProductViewPageMsg) >> ProductRepoMsg)
                    (GraphQLProduct.decoder "products")
                    (GraphQLProduct.encoder "products" GraphQLProduct.mock)
                , cmd
                ]
            )

        ProductViewPage.AddToCart id ->
            -- send to store later
            ( { model | cart = model.cart ++ [ id ] }
            , cmd
            )

        ProductViewPage.Noop ->
            ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ = Debug.log "msg" msg
    in
    case msg of
        ProductRepoMsg subMsg ->
                    let
                        ( newRepo, newMsg ) =
                            Repo.update subMsg model.products
                    in
                    update
                        newMsg
                        { model | products = newRepo }

        ProductListPageMsg subMsg ->
            case model.pageState of
                ProductListPageState state ->
                    let
                        ( newState, outMsg ) =
                            ProductListPage.update subMsg state
                    in
                    productListPageToUpdate
                        outMsg
                        ( { model | pageState = ProductListPageState newState }
                        , Cmd.none
                        )
                _ ->
                    ( model, Cmd.none )

        ProductViewPageMsg subMsg ->
            case model.pageState of
                ProductViewPageState state ->
                    let
                        ( newState, outMsg ) =
                            ProductViewPage.update subMsg state
                    in
                    productViewPageToUpdate
                        outMsg
                        ( { model | pageState = ProductViewPageState newState }
                        , Cmd.none
                        )
                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
