module Model exposing (Model, PageState(..))

import Browser.Navigation as Nav
import Dict exposing (Dict)
import GraphQLProduct
import ProductListPage
import ProductViewPage
import Repo exposing (Repo)


type alias Model =
    { pageState : PageState
    , products : Repo GraphQLProduct.Product
    , cart : List String
    , key : Nav.Key
    }


type PageState
    = LoadingPageState
    | NotFoundPageState
    | ProductListPageState ProductListPage.State
    | ProductViewPageState ProductViewPage.State
