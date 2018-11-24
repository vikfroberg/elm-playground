module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Route
import Test exposing (..)


suite : Test
suite =
    let
        _ =
            case Route.init of
                Route.ProductList ->
                    "Don't forget to implement test bellow"

                Route.ProductView id ->
                    "Don't forget to implement test bellow"
    in
    describe "App"
        [ describe "Route.toString"
            [ test "ProductList " <|
                \_ ->
                    Route.toString Route.ProductList
                        |> Route.fromString
                        |> Expect.equal (Just Route.ProductList)
            , test "ProductView" <|
                \_ ->
                    Route.toString (Route.ProductView "1")
                        |> Route.fromString
                        |> Expect.equal (Just (Route.ProductView "1"))
            ]
        ]
