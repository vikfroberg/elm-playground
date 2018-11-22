module GraphQL exposing (send, sendMock)

import Dict
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Process
import Task exposing (Task)


send : (Result Http.Error a -> msg) -> Decoder a -> String -> Cmd msg
send toMsg decoder query =
    Cmd.none


sendMock : (Result Http.Error a -> msg) -> Decoder a -> Value -> Cmd msg
sendMock toMsg decoder value =
    let
        res =
            { url = ""
            , status =
                { code = 200
                , message = ""
                }
            , headers = Dict.empty
            , body = ""
            }

        result =
            Decode.decodeValue decoder value
                |> Result.mapError Decode.errorToString
                |> Result.mapError (\s -> Http.BadPayload s res)
    in
    Process.sleep 2000
        |> Task.map (always result)
        |> Task.perform toMsg
