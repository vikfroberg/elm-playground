module Repo exposing (Msg(..), update, empty, get, Repo)

import Dict exposing (Dict)
import Http


type Repo a =
    Repo (Dict String a)


type Msg a msg
    = InsertMany (a -> String) (Result Http.Error (List String) -> msg) (Result Http.Error (List a))
    | Insert (a -> String) (Result Http.Error String -> msg) (Result Http.Error a)


update : Msg a msg -> Repo a -> ( Repo a, msg )
update msg (Repo dict) =
    case msg of
        InsertMany getId toMsg result ->
            case result of
                Ok xs ->
                    let
                        newDict =
                            List.foldl
                                (\x acc -> Dict.insert (getId x) x acc)
                                dict
                                xs
                    in
                    ( Repo newDict, toMsg (Ok <| List.map getId xs) )
                Err err ->
                    ( Repo dict, toMsg (Err err) )

        Insert getId toMsg result ->
            case result of
                Ok x ->
                    let
                        newDict =
                            Dict.insert (getId x) x dict
                    in
                    ( Repo newDict, toMsg (Ok <| getId x) )
                Err err ->
                    ( Repo dict, toMsg (Err err) )


empty : Repo a
empty = Repo Dict.empty


get : Repo a -> List String -> List a
get (Repo dict) ids =
    ids
        |> List.map (\id -> Dict.get id dict)
        |> List.filterMap identity


