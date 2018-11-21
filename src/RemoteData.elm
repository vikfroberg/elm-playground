module RemoteData exposing (map, RemoteData(..))

type RemoteData e a
    = NotAsked
    | Loading
    | Success a
    | Failure e


map : (a -> b) -> RemoteData e a -> RemoteData e b
map mapper data =
    case data of
        Success a ->
            Success (mapper a)
        Failure e ->
            Failure e
        NotAsked ->
            NotAsked
        Loading ->
            Loading
