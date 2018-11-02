module Async exposing (Async, Id, empty, push, race, set, takeFirst, takeLast, takeLatest)

import Task exposing (Task)
import Process
import Dict exposing (Dict)


type alias Id =
    Int


type alias Async a =
    State a


type alias State a =
    { pending : List Id
    , received : List Id
    , data : Dict Id a
    }


nextId : State a -> Id
nextId state =
    state.pending
        |> List.reverse
        |> List.head
        |> Maybe.map (\id -> id + 1)
        |> Maybe.withDefault 1


set : Id -> a -> State a -> State a
set id x state =
    { state
        | data = Dict.insert id x state.data
        , received = state.received ++ [ id ]
    }


push : (Id -> Cmd msg) -> State a -> ( State a, Cmd msg)
push toCmd state =
    let
        id =
            nextId state
    in
    ( { state | pending = state.pending ++ [ id ] }
    , toCmd id
    )


takeFirst : State a -> Maybe a
takeFirst state =
    state.received
        |> List.head
        |> Maybe.andThen (\id -> Dict.get id state.data)


takeLatest : State a -> Maybe a
takeLatest state =
    state.pending
        |> List.map (\id -> Dict.get id state.data)
        |> List.foldl maybeOr Nothing


race : State a -> Maybe a
race state  =
    state.received
        |> List.reverse
        |> List.head
        |> Maybe.andThen (\id -> Dict.get id state.data)


takeLast : State a -> Maybe a
takeLast state =
    state.pending
        |> List.reverse
        |> List.head
        |> Maybe.andThen (\id -> Dict.get id state.data)


empty : State a
empty =
    { pending = []
    , received = []
    , data = Dict.empty
    }


maybeOr : Maybe a -> Maybe a -> Maybe a
maybeOr ma mb =
    case ma of
        Nothing ->
            mb

        Just _ ->
            ma
