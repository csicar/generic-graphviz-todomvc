module App.Routes where

import Control.Alt ((<|>))
import Control.Applicative ((<*))
import Data.Array (last)
import Data.DotLang (class GraphRepr)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Functor ((<$))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.GenericGraph (class Edges, genericEdges, genericToGraph)
import Data.Maybe (fromMaybe)
import Data.Show (class Show)
import Data.String (Pattern(..), split)
import Pux.Router (end, router, lit)

data Route
  = All
  | Active
  | Completed
  | NotFound String

derive instance genericRoute :: Generic Route _

instance eqRoute :: Eq Route where eq = genericEq

instance showRoute :: Show Route where show = genericShow

instance graphReprRoute :: GraphRepr Route where toGraph = genericToGraph

instance edgesRoute :: Edges Route where edges x = genericEdges x

match :: String -> Route
match url' =
  fromMaybe (NotFound url') $ router url $
  All <$ end
  <|>
  Active <$ (lit "active") <* end
  <|>
  Completed <$ (lit "completed") <* end
  where
    url = fromMaybe url' $ last $ split (Pattern "/") url'

toURL :: Route -> String
toURL All = "./"
toURL Active = "./active"
toURL Completed = "./completed"
toURL (NotFound url) = url
