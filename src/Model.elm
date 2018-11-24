module Model exposing (Model, PageState(..))

import Dict exposing (Dict)
import GraphQLProduct
import ProductListPage
import ProductViewPage
import Repo exposing (Repo)


type alias Model =
    { pageState : PageState
    , products : Repo GraphQLProduct.Product
    , cart : List String
    }


type PageState
    = ProductListPageState ProductListPage.State
    | ProductViewPageState ProductViewPage.State
