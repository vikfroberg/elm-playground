module Main exposing (main)

import Browser
import Com exposing (Com(..))
import Html exposing (..)
import Html.Events exposing (onClick)
import Process
import ProductList exposing (viewConfig)
import RemoteData exposing (..)
import Task exposing (Task)
import Theme exposing (Theme)
import ViewEnv exposing (ViewEnv)


type alias Product =
    { title : String }


type alias Model =
    RemoteData (List Product)


type Msg
    = FetchProductResult (Result Never Product)
    | ProductList ProductList.Msg


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


fetchProduct : String -> Task Never Product
fetchProduct id =
    Process.sleep 2000
        |> Task.map (always { title = id })


init : Flags -> ( Model, Cmd Msg )
init _ =
    init_
        |> toCmd


init_ : ( Model, Com Msg )
init_ =
    ( NotAsked, ProductList.init )


toCmd : ( Model, Com Msg ) -> ( Model, Cmd Msg )
toCmd ( state, com ) =
    case com of
        EComCartAdd _ ->
            ( state, Cmd.none )

        FetchProduct name ->
            ( Loading
            , Task.attempt FetchProductResult (fetchProduct name)
            )

        Batch xs ->
            List.foldl
                (\x ( state_, cmd_ ) -> toCmd ( state, x ) |> Tuple.mapSecond ((\y -> y :: [ cmd_ ]) >> Cmd.batch))
                ( state, Cmd.none )
                xs

        None ->
            ( state, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    update_ msg model
        |> toCmd


update_ : Msg -> Model -> ( Model, Com Msg )
update_ msg model =
    case msg of
        ProductList subMsg ->
            ( model, ProductList.update subMsg )

        FetchProductResult result ->
            case result of
                Ok product ->
                    case model of
                        Success products ->
                            ( Success (product :: products), Com.none )

                        _ ->
                            ( Success [ product ], Com.none )

                Err _ ->
                    ( model, Com.none )



-- View


theme : Theme {}
theme =
    { primaryColor = "blue" }


view : Model -> Html Msg
view model =
    ProductList.view
        theme
        { width = 900 }
        model
        |> Html.map ProductList


viewCustom : Model -> Html Msg
viewCustom model =
    ProductList.viewAdvanced
        theme
        { viewConfig | viewCardItem = viewCardItemCustom }
        { width = 900 }
        model
        |> Html.map ProductList


viewCardItemCustom : Theme t -> ViewEnv v -> Product -> Html msg
viewCardItemCustom _ env product =
    div [] [ text ("Product name is '" ++ product.title ++ "'") ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
