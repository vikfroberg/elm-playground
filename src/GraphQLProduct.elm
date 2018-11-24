module GraphQLProduct exposing
    ( Product
    , decoderMany
    , decoderOne
    , getAllProductsMock
    , getAllProductsQuery
    , getProductsMock
    , getProductsQuery
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Product =
    { id : String
    , name : String
    }


getProductsMock : String -> List String -> Value
getProductsMock name ids =
    ids
        |> List.map (\id -> { id = id, name = "Product " ++ id })
        |> encoder name


getProductsQuery : List String -> String -> String
getProductsQuery ids name =
    let
        stringIds =
            ids
                |> String.join ","
    in
    """
    {{name}}: products(id: [{{ids}}]) {
        id
        name
    }
    """
        |> String.replace "{{ids}}" stringIds
        |> String.replace "{{name}}" name


getAllProductsMock : String -> Value
getAllProductsMock name =
    [ 1, 2, 3, 4, 5 ]
        |> List.map String.fromInt
        |> getProductsMock name


getAllProductsQuery : String -> String
getAllProductsQuery name =
    """
    {{name}}: products(id: {{id}}) {
        id
        name
    }
    """
        |> String.replace "{{name}}" name


decoderMany : String -> Decoder (List Product)
decoderMany name =
    Decode.field name <|
        Decode.list <|
            Decode.map2 Product
                (Decode.field "id" Decode.string)
                (Decode.field "name" Decode.string)


decoderOne : String -> Decoder Product
decoderOne name =
    Decode.field name <|
        Decode.index 0 <|
            Decode.map2 Product
                (Decode.field "id" Decode.string)
                (Decode.field "name" Decode.string)


encoder : String -> List Product -> Value
encoder name products =
    Encode.object
        [ ( name, Encode.list productEncoder products )
        ]


productEncoder : Product -> Value
productEncoder product =
    Encode.object
        [ ( "id", Encode.string product.id )
        , ( "name", Encode.string product.name )
        ]
