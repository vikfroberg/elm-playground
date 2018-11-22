module Model exposing (..)

import Dict exposing (Dict)
import ProductListPage
import ProductViewPage
import Repo exposing (Repo)


type alias Product =
    { id : String
    , name : String
    }


type alias Model =
    { pageState : PageState
    , products : Repo Product
    , cart : List String
    }


type PageState
    = ProductListPageState ProductListPage.State
    | ProductViewPageState ProductViewPage.State
