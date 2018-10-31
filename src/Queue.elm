module Queue exposing (Queue, empty, last, pop, push)


type alias Queue a =
    State a


type alias State a =
    { pending : Maybe a
    , queue : List a
    }


push : a -> State a -> ( State a, Maybe a )
push x state =
    case state.pending of
        Just _ ->
            ( { state | queue = state.queue ++ [ x ] }
            , Nothing
            )

        Nothing ->
            ( { state | pending = Just x }
            , Just x
            )


pop : State a -> ( State a, Maybe a )
pop state =
    case state.queue of
        [] ->
            ( { state | pending = Nothing }, Nothing )

        x :: xs ->
            ( { state | queue = xs, pending = Just x }
            , Just x
            )


last : State a -> Maybe a
last xs =
    Nothing


empty : State a
empty =
    { pending = Nothing, queue = [] }
