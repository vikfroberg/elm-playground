module Debounce exposing (Debounce, Id, empty, push, set)

import Task exposing (Task)
import Process
import Dict exposing (Dict)


type alias Id =
    Int


type alias Debounce a =
    State a


type alias State a =
    { maybeCurrent : Maybe Id
    , data : Dict Id a
    , order : List (Id, a)
    }


set : Id -> a -> State a -> State a
set id x state =
    { state
        | data = Dict.insert id x state.data
        , order = state.order ++ [ id ]
    }


-- debounce : msg -> State a -> ( State a, Cmd msg)
-- debounce toCmd state =
--     let
--         ( newState, cmd ) =
--             push toCmd state

--         debounceCmd =
--             Process.sleep state.debounceTime
--                 |> Task.map (always state.debounceTime)
--                 |> Task.perform state.msg
--     in
--     if state.isDebouncing then
--         ( newState
--         , Cmd.none
--         )
--     else
--         ( { newState | isDebouncing = True }
--         , Cmd.batch [cmd, debounceCmd ]
--         )


push : (Id -> Cmd msg) -> State a -> ( State a, Cmd msg)
push toCmd state =
    case state.maybeCurrent of
        Just current ->
            ( { state | maybeCurrent = Just (current + 1) }
            , toCmd (current + 1)
            )

        Nothing ->
            ( { state | maybeCurrent = Just 0 }
            , toCmd 0
            )


waitPush : (Id -> Cmd msg) -> State a -> ( State a, Maybe (Cmd msg) )
waitPush toCmd state =
    let
        currentData =
            state.maybeCurrent
                |> Maybe.map (\current -> Dict.get current state.data)
    in
    case state.maybeCurrent of
        Just current ->
            case Dict.get current state.data of
                Just _ ->
                    push toCmd state
                        |> Tuple.mapSecond Just

                Nothing ->
                    ( state, Nothing )

        Nothing ->
            push toCmd state
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
