module Async exposing (Async, Id, empty, push, race, set, takeFirst, takeLast, takeLatest, waitPush)

import Dict exposing (Dict)


type alias Id =
    Int


type alias Async a =
    State a


type alias State a =
    { maybeCurrent : Maybe Int
    , data : Dict Int a
    , order : List Int
    }


set : Int -> a -> State a -> State a
set id x state =
    { state
        | data = Dict.insert id x state.data
        , order = state.order ++ [ id ]
    }


push : (Int -> msg) -> State a -> ( State a, msg )
push msg state =
    case state.maybeCurrent of
        Just current ->
            ( { state | maybeCurrent = Just (current + 1) }
            , msg (current + 1)
            )

        Nothing ->
            ( { state | maybeCurrent = Just 0 }
            , msg 0
            )


waitPush : (Int -> msg) -> State a -> ( State a, Maybe msg )
waitPush msg state =
    let
        currentData =
            state.maybeCurrent
                |> Maybe.map (\current -> Dict.get current state.data)
    in
    case state.maybeCurrent of
        Just current ->
            case Dict.get current state.data of
                Just _ ->
                    push msg state
                        |> Tuple.mapSecond Just

                Nothing ->
                    ( state, Nothing )

        Nothing ->
            push msg state
                |> Tuple.mapSecond Just


takeFirst : State a -> Maybe a
takeFirst { maybeCurrent, data } =
    case maybeCurrent of
        Just current ->
            List.range 0 current
                |> List.map (\id -> Dict.get id data)
                |> List.foldr maybeOr Nothing

        Nothing ->
            Nothing


takeLatest : State a -> Maybe a
takeLatest { maybeCurrent, data } =
    case maybeCurrent of
        Just current ->
            List.range 0 current
                |> List.map (\id -> Dict.get id data)
                |> List.foldl maybeOr Nothing

        Nothing ->
            Nothing


race : State a -> Maybe a
race { data, order } =
    order
        |> List.map (\id -> Dict.get id data)
        |> List.foldl maybeOr Nothing


takeLast : State a -> Maybe a
takeLast { maybeCurrent, data } =
    case maybeCurrent of
        Just current ->
            Dict.get current data

        Nothing ->
            Nothing


empty : State a
empty =
    { maybeCurrent = Nothing
    , data = Dict.empty
    , order = []
    }


maybeOr : Maybe a -> Maybe a -> Maybe a
maybeOr ma mb =
    case ma of
        Nothing ->
            mb

        Just _ ->
            ma
