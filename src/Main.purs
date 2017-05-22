module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (doctype)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, pure, unit, void, ($))
import React (Event, ReactClass, createClass, createFactory, spec)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)

initialState :: Unit
initialState = unit

loginWithGoogle :: forall eff. Event -> Eff (console :: CONSOLE | eff) Unit
loginWithGoogle e = do 
  log "Login"

app :: forall props. ReactClass props
app = createClass $ spec initialState \ctx -> do
  pure $ 
    D.div [ P.className "container" ] 
          [ D.h3' [ D.text "Calibrate me!" ]
          , D.button [ P.onClick loginWithGoogle ]
                     [ D.text "Login with google" ]
          ]

main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = void do
  log "Hello sailor!"
  let component = createFactory app unit
  w <- window
  hdoc <- document w
  let doc = htmlDocumentToDocument hdoc
      node = documentToNonElementParentNode doc
      eid = ElementId "main"
  ctr <- getElementById eid node
  let justCtr = unsafePartial fromJust ctr
  render component justCtr

