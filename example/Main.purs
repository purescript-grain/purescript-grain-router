module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Grain (class GlobalGrain, GProxy(..), VNode, fromConstructor, mount, useValue)
import Grain.Markup as H
import Grain.Router (class Router, initialRouter, link, useRouter)
import Grain.Router.Parser (end, int, lit, match, param)
import Web.DOM.Element (toNode)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

data Route
  = Home
  | User Int
  | Users String
  | NotFound

instance routerRoute :: Router Route where
  parse path =
    fromMaybe NotFound $ match path $
      Home <$ end
      <|>
      Users <$> (lit "users" *> param "name") <* end
      <|>
      User <$> (lit "users" *> int) <* end

instance globalGrainRoute :: GlobalGrain Route where
  initialState _ = initialRouter
  typeRefOf _ = fromConstructor NotFound

main :: Effect Unit
main = do
  maybeEl <- window >>= document <#> toParentNode >>= querySelector (QuerySelector "#app")
  case maybeEl of
    Nothing -> pure unit
    Just el ->
      mount view $ toNode el

view :: VNode
view = H.component do
  route <- useValue (GProxy :: _ Route)
  startRouter <- useRouter (GProxy :: _ Route)
  pure $ H.div
    # H.didCreate (const startRouter)
    # H.kids
        [ case route of
            Home ->
              H.key "home" $ H.div # H.kids
                [ H.h1 # H.kids [ H.text "Home" ]
                , link "/users/51" # H.kids [ H.text "To user 51" ]
                ]
            User i ->
              H.key "user" $ H.div # H.kids
                [ H.h1 # H.kids [ H.text $ "User " <> show i ]
                , link "/users?name=ichiro" # H.kids [ H.text "To ichiro" ]
                ]
            Users name ->
              H.key "users" $ H.div # H.kids
                [ H.h1 # H.kids [ H.text $ "I am " <> name ]
                , link "/not_found" # H.kids [ H.text "Somewhere" ]
                ]
            NotFound ->
              H.key "notFound" $ H.div # H.kids
                [ H.h1 # H.kids [ H.text "NotFound" ]
                , link "/" # H.kids [ H.text "To Home" ]
                ]
        ]
