module TupleExtra exposing (applyl, applyr, foldl)


applyr : (b -> a -> c) -> ( a, b ) -> c
applyr fn ( first, second ) =
    fn second first


applyl : (a -> b -> c) -> ( a, b ) -> c
applyl fn ( first, second ) =
    fn first second


foldl : (x -> y -> ( y, z )) -> y -> List x -> ( y, List z )
foldl fn y xs =
    let
        folder x ( newModel, zs ) =
            fn x newModel
                |> Tuple.mapSecond (\z -> zs ++ [ z ])
    in
    List.foldl
        folder
        ( y, [] )
        xs
