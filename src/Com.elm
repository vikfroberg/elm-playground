module Com exposing (Com(..), batch, map, none)


type Com msg
    = FetchProduct String
    | EComCartAdd String
    | Batch (List (Com msg))
    | None


none : Com msg
none =
    None


batch : List (Com msg) -> Com msg
batch xs =
    Batch xs


map : (a -> msg) -> Com a -> Com msg
map fn msg =
    case msg of
        EComCartAdd x ->
            EComCartAdd x

        FetchProduct url ->
            FetchProduct url

        Batch xs ->
            Batch (List.map (map fn) xs)

        None ->
            None
