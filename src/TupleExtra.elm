module TupleExtra exposing (applyl, applyr, foldlSecond)


applyr : (b -> a -> c) -> ( a, b ) -> c
applyr fn ( first, second ) =
    fn second first


applyl : (a -> b -> c) -> ( a, b ) -> c
applyl fn ( first, second ) =
    fn first second


foldlSecond : (x -> y -> ( y, z )) -> ( y, List x ) -> ( y, List z )
foldlSecond fn ( y, xs ) =
    let
        folder x ( newY, zs ) =
            fn x newY
                |> Tuple.mapSecond (\z -> zs ++ [ z ])
    in
    List.foldl
        folder
        ( y, [] )
        xs
