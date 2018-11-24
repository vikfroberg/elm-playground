module Route exposing (Route(..), fromString, init, toString)

import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = ProductList
    | ProductView String


init =
    ProductList


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map ProductList Parser.top
        , Parser.map ProductView (s "products" </> string)
        ]


fromString : String -> Maybe Route
fromString str =
    Url.fromString ("http://site.com" ++ str)
        |> Maybe.andThen (Parser.parse parser)


toString : Route -> String
toString route =
    case route of
        ProductList ->
            "/"

        ProductView id ->
            "/products/" ++ id
