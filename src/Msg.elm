module Msg exposing (..)

import ProductListPage
import ProductViewPage
import Http
import Model exposing (..)
import Repo


type Msg
    = ProductListPageMsg ProductListPage.Msg
    | ProductRepoMsg (Repo.Msg Product Msg)
    | ProductViewPageMsg ProductViewPage.Msg
