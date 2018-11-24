module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import GraphQL
import GraphQLProduct
import Model exposing (..)
import Msg exposing (..)
import ProductListPage
import ProductViewPage
import Repo exposing (Repo)
import Route
import Router
import TupleExtra as Tuple
import Url exposing (Url)
import View exposing (..)



-- TEA


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view =
            \model ->
                { title = "App", body = [ view model ] }
        , update =
            \msg model ->
                ( msg, update msg model )
                    |> Debug.log "Action"
                    |> Tuple.second
        , subscriptions = subscriptions
        , onUrlChange = Router.ChangedUrl >> RouterMsg
        , onUrlRequest = Router.ClickedLink >> RouterMsg
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        _ =
            Debug.log "init" url

        model =
            { products = Repo.empty
            , pageState = LoadingPageState
            , cart = []
            , key = key
            }
    in
    update (RouterMsg (Router.ChangedUrl url)) model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouterMsg subMsg ->
            case Router.update subMsg of
                Router.Noop ->
                    ( model, Cmd.none )

                Router.PushUrl path ->
                    ( model, Nav.pushUrl model.key path )

                Router.LoadUrl href ->
                    ( model
                    , Nav.load href
                    )

                Router.ChangeRoute maybeRoute ->
                    case maybeRoute of
                        Nothing ->
                            ( { model | pageState = NotFoundPageState }
                            , Cmd.none
                            )

                        Just route ->
                            case route of
                                Route.ProductList ->
                                    ProductListPage.init
                                        |> Tuple.mapFirst (ProductListPageState >> setPageState model)
                                        |> Tuple.foldlSecond productListPageUpdate
                                        |> Tuple.mapSecond Cmd.batch

                                Route.ProductView id ->
                                    ProductViewPage.init id
                                        |> Tuple.mapFirst (ProductViewPageState >> setPageState model)
                                        |> Tuple.foldlSecond productViewPageUpdate
                                        |> Tuple.mapSecond Cmd.batch

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
    let
        _ =
            Debug.log "ProductListPage.OutMsg" outMsg
    in
    case outMsg of
        ProductListPage.LoadProducts toMsg ->
            ( model
            , GraphQL.sendMock
                (GraphQLProduct.decoderMany "products")
                (GraphQLProduct.getAllProductsMock "products")
                (Repo.insertMany .id (toMsg >> ProductListPageMsg))
                |> Cmd.map ProductRepoMsg
            )

        ProductListPage.AddToCart id ->
            -- send to store later
            ( { model | cart = model.cart ++ [ id ] }
            , Cmd.none
            )


productViewPageUpdate : ProductViewPage.OutMsg -> Model -> ( Model, Cmd Msg )
productViewPageUpdate outMsg model =
    let
        _ =
            Debug.log "ProductViewPage.OutMsg" outMsg
    in
    case outMsg of
        ProductViewPage.LoadProduct id toMsg ->
            let
                toCmd =
                    GraphQL.sendMock
                        (GraphQLProduct.decoderOne "products")
                        (GraphQLProduct.getProductsMock "products" [ id ])

                config =
                    { toId = .id
                    , toMsg = toMsg >> ProductViewPageMsg
                    , toCmd = toCmd
                    }
            in
            ( model
            , Repo.fetchOne
                config
                id
                model.products
                |> Cmd.map ProductRepoMsg
            )

        ProductViewPage.ReloadProduct id toMsg ->
            let
                toCmd =
                    GraphQL.sendMock
                        (GraphQLProduct.decoderOne "products")
                        (GraphQLProduct.getProductsMock "products" [ id ])
            in
            ( model
            , toCmd (Repo.insertOne .id (toMsg >> ProductViewPageMsg))
                |> Cmd.map ProductRepoMsg
            )

        ProductViewPage.AddToCart id ->
            -- send to store later
            ( { model | cart = model.cart ++ [ id ] }
            , Cmd.none
            )
