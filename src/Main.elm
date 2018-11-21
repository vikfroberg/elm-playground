module Main exposing (main)

import Browser
import Html exposing (..)
import Process
import ProductListPage
import ProductViewPage
import Task exposing (Task)
import Dict exposing (Dict)


type alias Product =
    { id : Int
    , name : String
    }


type alias Model =
    { pageState : PageState
    , products : Dict Int Product
    , cart : List Int
    }


type PageState
    = ProductListPageState ProductListPage.State
    | ProductViewPageState ProductViewPage.State


type Msg
    = ProductListPageMsg ProductListPage.Msg
    | ProductViewPageMsg ProductViewPage.Msg
    | ReceviedProducts (List Int -> Msg) (List Product)


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


delay : a -> Task Never a
delay x =
    Process.sleep 2000
        |> Task.map (always x)


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        ( productListPageState, productListPageOutMsg ) =
            ProductListPage.init
    in
    productListPageToUpdate
        productListPageOutMsg
        ( { pageState = ProductListPageState productListPageState
          , products = Dict.empty
          , cart = []
          }
        , Cmd.none
        )


productListPageToUpdate : ProductListPage.OutMsg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
productListPageToUpdate outMsg ( model, cmd ) =
    case outMsg of
        ProductListPage.LoadProducts toMsg ->
            ( model
            , Cmd.batch
                [ Task.perform
                    (ReceviedProducts (toMsg >> ProductListPageMsg))
                    (delay [{ id = 1, name = "Product 1" }])
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
        ProductViewPage.LoadProducts ids toMsg ->
            ( model
            , Cmd.batch
                [ Task.perform
                    (ReceviedProducts (toMsg >> ProductViewPageMsg))
                    (delay (List.map (\id -> { id = id, name = "Product " ++ String.fromInt id }), ids))
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
        _ = Debug.log "update" msg
    in
    case msg of
        ReceviedProducts toMsg products ->
            let
                newProducts =
                    List.foldl
                        (\product acc -> Dict.insert product.id product acc)
                        model.products
                        products

                productIds =
                    List.map .id products

                ( newModel, newCmd ) = update (toMsg productIds) model
            in
            ( { newModel | products = newProducts } , newCmd )

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


getProducts : Dict Int Product -> List Int -> List Product
getProducts products ids =
    ids
        |> List.map (\id -> Dict.get id products)
        |> List.filterMap identity


view : Model -> Html Msg
view model =
    let
        content =
            case model.pageState of
                ProductListPageState state ->
                    ProductListPage.view
                        { getProducts = (getProducts model.products) }
                        state
                        |> Html.map ProductListPageMsg
    in
        div []
            [ text <| "Items in cart: " ++ String.fromInt (List.length model.cart)
            , content
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
