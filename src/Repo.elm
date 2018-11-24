module Repo exposing (Msg, Repo, empty, getMany, getOne, insertMany, insertOne, update)

import Dict exposing (Dict)
import Http


type Repo a
    = Repo (Dict String a)


type Msg a msg
    = Merge msg (Result Http.Error (Dict String a))


insertOne : (Result Http.Error String -> msg) -> (a -> String) -> Result Http.Error a -> Msg a msg
insertOne toMsg toId result =
    let
        toDict a =
            Dict.empty
                |> Dict.insert (toId a) a
    in
    Merge
        (toMsg (Result.map toId result))
        (Result.map toDict result)


insertMany : (Result Http.Error (List String) -> msg) -> (a -> String) -> Result Http.Error (List a) -> Msg a msg
insertMany toMsg toId result =
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
