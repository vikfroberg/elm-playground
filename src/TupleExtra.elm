module TupleExtra exposing (..)


applyr fn ( first, second ) =
    fn second first

applyl fn ( first, second ) =
    fn first second


