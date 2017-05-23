module Main where

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Firebase (FIREBASE, Options, initializeApp)
import Firebase.Authentication as FBA
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, const, discard, pure, show, unit, void, ($), (<<<))
import React (ComponentDidMount, Event, ReactClass, ReactElement, ReactSpec, ReactState, ReactThis, ReadWrite, Render, createClass, createFactory, readState, writeState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)

firebaseOptions :: Options
firebaseOptions = {
  apiKey: "AIzaSyBf1CNFG4xkCEgvWFJ0BMeNoGAQ73bhdcw",
  authDomain: "purescript-5ace3.firebaseapp.com",
  databaseURL: "https://purescript-5ace3.firebaseio.com",
  projectId: "purescript-5ace3",
  storageBucket: "purescript-5ace3.appspot.com",
  messagingSenderId: "399241446371"
}

initialState :: Boolean
initialState = false

loginWithGoogle :: forall eff. Event -> Eff (firebase :: FIREBASE, console :: CONSOLE | eff) Unit
loginWithGoogle e = do
  log "Login"
  provider <- FBA.newGoogleProvider
  _ <- runAff (log <<< show) (const $ pure unit) do
    liftEff $ log "Signing in"
    FBA.signInWithPopup provider
  pure unit

logout :: forall eff. Event -> Eff (firebase :: FIREBASE, console :: CONSOLE | eff) Unit
logout e = do
  log "Logout"
  FBA.signOut

spec'' :: forall props state eff.
  state -> ComponentDidMount props state eff -> Render props state eff -> ReactSpec props state eff
spec'' state componentDidMount render =
  { render
  , displayName: ""
  , getInitialState: const $ pure state
  , componentWillMount: const $ pure unit
  , componentDidMount
  , componentWillReceiveProps: \_ _ -> pure unit
  , shouldComponentUpdate: \_ _ _ -> pure true
  , componentWillUpdate: \_ _ _ -> pure unit
  , componentDidUpdate: \_ _ _ -> pure unit
  , componentWillUnmount: \_ -> pure unit
  }

didMount :: forall props eff.
  ReactThis props Boolean ->
  Eff (firebase :: FIREBASE, state :: ReactState ReadWrite | eff) Unit
didMount ctx = do
  FBA.onAuthStateChanged \loggedIn -> do
    _ <- writeState ctx loggedIn
    pure unit

login :: Array ReactElement
login =
          [ D.h3' [ D.text "Calibrate me!" ]
          , D.button [ P.onClick loginWithGoogle ]
                     [ D.text "Login with google" ]
          ]

cards :: Array ReactElement
cards =
          [ D.h3' [ D.text "Calibrate me!" ]
          , D.button [ P.onClick logout ]
                     [ D.text "Logout" ]
          ]

app :: forall props. ReactClass props
app = createClass $ spec'' initialState didMount \ctx -> do
  loggedIn <- readState ctx
  log $ show loggedIn
  pure $
    D.div [ P.className "container" ]
      if loggedIn then cards else login

main :: forall e. Eff (console :: CONSOLE, dom :: DOM, firebase :: FIREBASE | e) Unit
main = void do
  log "Hello sailor!"
  initializeApp firebaseOptions
  let component = createFactory app unit
  w <- window
  hdoc <- document w
  let doc = htmlDocumentToDocument hdoc
      node = documentToNonElementParentNode doc
      eid = ElementId "main"
  ctr <- getElementById eid node
  let justCtr = unsafePartial fromJust ctr
  render component justCtr