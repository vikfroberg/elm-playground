module RemoteData exposing (RemoteData(..))


type RemoteData a
    = NotAsked
    | Loading
    | Success a
