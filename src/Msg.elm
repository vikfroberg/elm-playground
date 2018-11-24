module Msg exposing (Msg(..))

import GraphQLProduct
import Http
import Model exposing (..)
import ProductListPage
import ProductViewPage
import Repo


type Msg
    = ProductListPageMsg ProductListPage.Msg
    | ProductRepoMsg (Repo.Msg GraphQLProduct.Product Msg)
    | ProductViewPageMsg ProductViewPage.Msg
