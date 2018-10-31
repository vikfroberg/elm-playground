module Test exposing (main)

import Async exposing (Async)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Process
import Queue exposing (Queue)
import Task exposing (Task)


type alias Model =
    { search : String
    , get : Async Int
    , post : Async Int
    , queue : Queue (Cmd Msg)
    , queueState : Maybe Int
    }


type Msg
    = GetInt Async.Id Int
    | PostInt Async.Id Int
    | QueueInt Int


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


fetchInt : Float -> Int -> Task Never Int
fetchInt sec n =
    Process.sleep (sec * 1000)
        |> Task.map (always n)


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        -- GET
        ( get, getMsg ) =
            Async.push GetInt Async.empty

        ( get2, getMsg2 ) =
            Async.push GetInt get

        ( get3, getMsg3 ) =
            Async.push GetInt get2

        ( get4, getMsg4 ) =
            Async.push GetInt get3

        -- POST
        ( post, postMsg ) =
            Async.waitPush PostInt Async.empty

        ( post2, postMsg2 ) =
            Async.waitPush PostInt post

        -- QUEUE
        ( queue, queueCmd ) =
            Queue.push
                (Task.perform QueueInt (fetchInt 1 1))
                Queue.empty

        ( queue2, queueCmd2 ) =
            Queue.push
                (Task.perform QueueInt (fetchInt 1 2))
                queue

        ( queue3, queueCmd3 ) =
            Queue.push
                (Task.perform QueueInt (fetchInt 1 3))
                queue2
    in
    ( { search = ""
      , get = get4
      , post = post2
      , queue = Debug.log "queue" queue3
      , queueState = Nothing
      }
    , Cmd.batch
        [ Task.perform getMsg (fetchInt 1 1)
        , Task.perform getMsg2 (fetchInt 4 2)
        , Task.perform getMsg3 (fetchInt 2 3)
        , Task.perform getMsg4 (fetchInt 3 4)
        , postMsg
            |> Maybe.map (\m -> Task.perform m (fetchInt 1 1))
            |> Maybe.withDefault Cmd.none
        , postMsg2
            |> Maybe.map (\m -> Task.perform m (fetchInt 2 2))
            |> Maybe.withDefault Cmd.none
        , queueCmd
            |> Maybe.withDefault Cmd.none
        , queueCmd2
            |> Maybe.withDefault Cmd.none
        , queueCmd3
            |> Maybe.withDefault Cmd.none
        ]
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetInt id n ->
            ( mapGet (Async.set id n) model
            , Cmd.none
            )

        PostInt id n ->
            ( mapPost (Async.set id n) model
            , Cmd.none
            )

        QueueInt n ->
            let
                ( queue, cmd ) =
                    Queue.pop model.queue
                        |> Tuple.mapSecond (Maybe.withDefault Cmd.none)
            in
            ( { model | queueState = Just n, queue = queue }
            , cmd
            )


mapGet : (Async Int -> Async Int) -> Model -> Model
mapGet fn model =
    { model | get = fn model.get }


mapPost : (Async Int -> Async Int) -> Model -> Model
mapPost fn model =
    { model | post = fn model.post }



-- View


view : Model -> Html Msg
view model =
    let
        first =
            Async.takeFirst model.get
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "Loading"

        latest =
            Async.takeLatest model.get
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "Loading"

        last =
            Async.takeLast model.get
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "Loading"

        race =
            Async.race model.get
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "Loading"

        racePost =
            Async.race model.post
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "Loading"

        queue =
            model.queueState
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "Loading"
    in
    div []
        [ div []
            [ h2 [] [ text "Get" ]
            , div [] [ text ("takeFirst: " ++ first) ]
            , div [] [ text ("takeLatest: " ++ latest) ]
            , div [] [ text ("takeLast: " ++ last) ]
            , div [] [ text ("takeRace: " ++ race) ]
            ]
        , div []
            [ h2 [] [ text "Post" ]
            , div [] [ text ("takeRace: " ++ racePost) ]
            ]
        , div []
            [ h2 [] [ text "Queue" ]
            , div [] [ text ("queue: " ++ queue) ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Helpers
