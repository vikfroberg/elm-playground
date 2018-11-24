module Repo exposing
    ( Msg
    , Repo
    , empty
    , fetchOne
    , getMany
    , getOne
    , insertMany
    , insertOne
    , update
    )

import Dict exposing (Dict)
import Http
import Task


type Repo a
    = Repo (Dict String a)


type Msg a msg
    = Merge msg (Result Http.Error (Dict String a))
    | Noop msg


fetchOne :
    { toId : a -> String
    , toMsg : Result Http.Error String -> msg
    , toCmd : (Result Http.Error a -> Msg a msg) -> Cmd (Msg a msg)
    }
    -> String
    -> Repo a
    -> Cmd (Msg a msg)
fetchOne config id repo =
    case getOne repo id of
        Just a ->
            config.toMsg (Ok id)
                |> Noop
                |> Task.succeed
                |> Task.perform identity

        Nothing ->
            insertOne config.toId config.toMsg
                |> config.toCmd


insertOne : (a -> String) -> (Result Http.Error String -> msg) -> Result Http.Error a -> Msg a msg
insertOne toId toMsg result =
    let
        toDict a =
            Dict.empty
                |> Dict.insert (toId a) a
    in
    Merge
        (toMsg (Result.map toId result))
        (Result.map toDict result)


insertMany : (a -> String) -> (Result Http.Error (List String) -> msg) -> Result Http.Error (List a) -> Msg a msg
insertMany toId toMsg result =
    let
        toDict =
            List.map (\a -> ( toId a, a )) >> Dict.fromList

        toIds =
            List.map toId
    in
    Merge
        (toMsg (Result.map toIds result))
        (Result.map toDict result)


update : Msg a msg -> Repo a -> ( Repo a, msg )
update msg (Repo dict) =
    case msg of
        Noop newMsg ->
            ( Repo dict, newMsg )

        Merge newMsg result ->
            case result of
                Ok newDict ->
                    let
                        mergedDict =
                            Dict.union newDict dict
                    in
                    ( Repo mergedDict, newMsg )

                Err err ->
                    ( Repo dict, newMsg )


empty : Repo a
empty =
    Repo Dict.empty


getMany : Repo a -> List String -> List a
getMany (Repo dict) ids =
    ids
        |> List.map (\id -> Dict.get id dict)
        |> List.filterMap identity


getOne : Repo a -> String -> Maybe a
getOne repo id =
    [ id ]
        |> getMany repo
        |> List.head



-- Helpers
