module Repo exposing (Msg(..), Repo, empty, get, getMany, update)

import Dict exposing (Dict)
import Http


type Repo a
    = Repo (a -> String) (Dict String a)


type Msg a msg
    = InsertMany (Result Http.Error (List String) -> msg) (Result Http.Error (List a))
    | Insert (Result Http.Error String -> msg) (Result Http.Error a)


update : Msg a msg -> Repo a -> ( Repo a, msg )
update msg (Repo getId dict) =
    case msg of
        InsertMany toMsg result ->
            case result of
                Ok xs ->
                    let
                        newDict =
                            List.foldl
                                (\x acc -> Dict.insert (getId x) x acc)
                                dict
                                xs
                    in
                    ( Repo getId newDict, toMsg (Ok <| List.map getId xs) )

                Err err ->
                    ( Repo getId dict, toMsg (Err err) )

        Insert toMsg result ->
            case result of
                Ok x ->
                    let
                        newDict =
                            Dict.insert (getId x) x dict
                    in
                    ( Repo getId newDict, toMsg (Ok <| getId x) )

                Err err ->
                    ( Repo getId dict, toMsg (Err err) )


empty : (a -> String) -> Repo a
empty getId =
    Repo getId Dict.empty


getMany : Repo a -> List String -> List a
getMany (Repo getIds dict) ids =
    ids
        |> List.map (\id -> Dict.get id dict)
        |> List.filterMap identity


get : Repo a -> String -> Maybe a
get repo id =
    [ id ]
        |> getMany repo
        |> List.head
