module GraphQLProduct exposing (decoder, decoderMany, encoder, getAllProductsQuery, getProductsQuery, mock)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Product =
    { id : String
    , name : String
    }


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


decoder : String -> Decoder Product
decoder name =
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


mock : List Product
mock =
    [ { id = "1", name = "Product 1" }
    , { id = "2", name = "Product 2" }
    , { id = "3", name = "Product 3" }
    , { id = "4", name = "Product 4" }
    ]
