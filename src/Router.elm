module Router exposing (Msg(..), OutMsg(..), fromUrl, href, update)

import Browser
import Html exposing (Attribute)
import Html.Attributes as Attr
import Route exposing (Route)
import Url exposing (Url)


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest


type OutMsg
    = PushUrl String
    | LoadUrl String
    | ChangeRoute (Maybe Route)
    | Noop


update : Msg -> OutMsg
update msg =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            Noop

                        Just _ ->
                            PushUrl (Url.toString url)

                Browser.External url ->
                    LoadUrl url

        ChangedUrl url ->
            ChangeRoute (fromUrl url)


href : Route -> Attribute msg
href route =
    Attr.href ("#" ++ Route.toString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Maybe.withDefault "" url.fragment
        |> Route.fromString
