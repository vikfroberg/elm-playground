module ProductList exposing (Msg, init, update, view, viewAdvanced, viewConfig)

import Com exposing (Com(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import RemoteData exposing (..)
import Theme exposing (Theme)
import ViewEnv exposing (ViewEnv)

type alias Product p =
    { p | title : String }


type alias ViewConfig t p v =
    { viewCardItem : Theme t -> ViewEnv v -> Product p -> Html Msg
    , viewPageTitle : Theme t -> ViewEnv v -> Html Msg
    }


init : Com msg
init =
    FetchProduct "Old one"



type Msg
    = LoadNew


update : Msg -> OutMsg
update msg =
    case msg of
        LoadNew ->
            FetchProduct "New one"


viewConfig : ViewConfig t p v
viewConfig =
    { viewCardItem = viewCardItem
    , viewPageTitle = viewPageTitle
    }


view : Theme t -> ViewEnv v -> RemoteData (List (Product p)) -> Html Msg
view theme env =
    viewAdvanced theme viewConfig env


viewAdvanced : Theme t -> ViewConfig t p v -> ViewEnv v -> RemoteData (List (Product p)) -> Html Msg
viewAdvanced theme config env remoteProducts =
    div []
        [ config.viewPageTitle theme env
        , viewList theme config env remoteProducts
        ]


viewList : Theme t -> ViewConfig t p v -> ViewEnv v -> RemoteData (List (Product p)) -> Html Msg
viewList theme config env remoteProducts =
    case remoteProducts of
        NotAsked ->
            div [] [ text "Waiting to fetch products" ]

        Loading ->
            div [] [ text "Loading..." ]

        Success products ->
            div []
                [ div [ onClick LoadNew ] [ text "Load new" ]
                , div [] (List.map (config.viewCardItem theme env) products)
                ]


viewCardItem : Theme t -> ViewEnv v -> Product p -> Html Msg
viewCardItem theme env product =
    div [] [ text product.title ]


viewPageTitle : Theme t -> ViewEnv v -> Html Msg
viewPageTitle theme env =
    h1
        [ style "color" theme.primaryColor ]
        [ text "Products" ]
