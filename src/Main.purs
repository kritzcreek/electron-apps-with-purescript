module Main where

import Prelude
import React as R
import ReactDOM as RDOM
import Thermite as T
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (try)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Either (either)
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import Node.FS (FS)
import Node.FS.Sync (readdir)
import Partial.Unsafe (unsafePartial)
import React.DOM (text, li', ul')

type FileNames = {names :: Array String}

dirListingComponent
  :: forall eff. T.Spec eff Unit FileNames Unit
dirListingComponent =
  T.simpleSpec T.defaultPerformAction render
  where render _ props _ _ =
          [ ul'
            (map (\file -> li' [text file]) props.names)
          ]

main :: Eff (fs :: FS, dom :: DOM) Unit
main = void do
  fileNames <- either (const []) id <$> try (readdir ".")
  let component = T.createClass dirListingComponent unit
  document <- DOM.window >>= DOM.document
  container <-
    unsafePartial
    (fromJust <<< toMaybe
    <$> DOM.querySelector "body"
    (DOM.htmlDocumentToParentNode document))
  RDOM.render
    (R.createFactory component ({names: fileNames}))
    container
