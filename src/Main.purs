module Main where

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Either (Either(Left, Right))
import Data.Foldable (for_)
import Data.Foreign (Foreign, ForeignError, F, readString, toForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Data.Foreign.Index (readProp)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (round)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (fromJust)
import Debug.Trace (traceAnyA)
import Firebase (FIREBASE, Options, initializeApp)
import Firebase.Authentication as FBA
import Firebase.Database (onValue, push)
import Global (readFloat, readInt)
import Partial.Unsafe (unsafePartial)
import Prelude (class Show, Unit, bind, const, discard, map, pure, show, unit, void, ($), (<<<))
import React (ComponentDidMount, Event, ReactClass, ReactElement, ReactSpec, ReactState, ReactThis, ReadWrite, Render, createClass, createFactory, readState, transformState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)
import Unsafe.Coerce (unsafeCoerce)

firebaseOptions :: Options
firebaseOptions = {
  apiKey: "AIzaSyBf1CNFG4xkCEgvWFJ0BMeNoGAQ73bhdcw",
  authDomain: "purescript-5ace3.firebaseapp.com",
  databaseURL: "https://purescript-5ace3.firebaseio.com",
  projectId: "purescript-5ace3",
  storageBucket: "purescript-5ace3.appspot.com",
  messagingSenderId: "399241446371"
}

type State =
  { loggedIn :: Boolean
  , predictions :: Array Prediction
  , currentPrediction :: Prediction
  }

type Predictions = Array Prediction

newtype Prediction = Prediction
  { name :: String
  , probability :: Int
  , correct :: Correctness
  }

data Correctness = Correct | Incorrect | Unknown

derive instance genericCorrectness :: Generic Correctness _
derive instance genericPrediction :: Generic Prediction _

instance showCorrectness :: Show Correctness where
  show = genericShow

ge :: forall a rep. Generic a rep => GenericEncode rep => a -> Foreign
ge = genericEncode $ defaultOptions

instance encodePrediction :: Encode Prediction where
  encode = ge

instance encodeCorrectness :: Encode Correctness where
  encode = ge

de :: forall a rep. Generic a rep => GenericDecode rep => Foreign -> F a
de = genericDecode $ defaultOptions

instance decodePrediction :: Decode Prediction where
  decode = de

instance decodeCorrectness :: Decode Correctness where
  decode = de

emptyPrediction :: Prediction
emptyPrediction = Prediction {name: "", probability: 50, correct: Unknown}

initialState :: State
initialState =
  { loggedIn: false
  , predictions: []
  , currentPrediction: emptyPrediction
  }

loginWithGoogle :: forall eff. Event -> Eff (firebase :: FIREBASE, console :: CONSOLE | eff) Unit
loginWithGoogle e = void do
  provider <- FBA.newGoogleProvider
  runAff (log <<< show) (const $ pure unit) (FBA.signInWithPopup provider)

logout :: forall eff. Event -> Eff (firebase :: FIREBASE | eff) Unit
logout e = do
  FBA.signOut

addNewItem :: forall props eff. ReactThis props State -> Event
  -> Eff (state :: ReactState ReadWrite, firebase :: FIREBASE | eff) Unit
addNewItem ctx e = do
  {currentPrediction} <- readState ctx
  push "predictions" (encode currentPrediction)
  transformState ctx (\state -> state { currentPrediction = resetPrediction state.currentPrediction })

resetPrediction :: Prediction -> Prediction
resetPrediction (Prediction p) = Prediction $ p { name = "" }

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
  ReactThis props State ->
  Eff (firebase :: FIREBASE, state :: ReactState ReadWrite | eff) Unit
didMount ctx = do
  FBA.onAuthStateChanged \loggedIn -> do
    transformState ctx (\s -> s {loggedIn = loggedIn})
    if loggedIn then
      void $ onValue "predictions" (receivePredictions ctx)
      else pure unit

receivePredictions :: forall props eff. ReactThis props State -> Foreign
  -> Eff (firebase :: FIREBASE, state :: ReactState ReadWrite | eff) Unit
receivePredictions ctx j = case runExcept $ decode $ values j of
  Left x -> traceAnyA $ show x
  Right v ->
    transformState ctx (\s -> s {predictions = v})

foreign import values :: Foreign -> Foreign
foreign import stringify :: forall a. a -> String

updateName :: String -> Prediction -> Prediction
updateName n (Prediction p) = Prediction $ p { name = n }

updateProbability :: String -> Prediction -> Prediction
updateProbability x (Prediction p) = Prediction $ p { probability = round $ readFloat x }

updateCorrectness :: String -> Prediction -> Prediction
updateCorrectness x (Prediction p) = Prediction $ p { correct = fromString x }
  where fromString :: String -> Correctness
        fromString "Correct" = Correct
        fromString "Incorrect" = Incorrect
        fromString _ = Unknown

type Handler = forall props eff
  .  ReactThis props State
  -> Event
  -> Eff (state :: ReactState ReadWrite | eff) Unit

inputChanged :: Handler
inputChanged ctx e =
  for_ (valueOf e) \name ->
    transformState ctx (\state -> state { currentPrediction = updateName name state.currentPrediction })

probabilityChanged :: Handler
probabilityChanged ctx e =
  for_ (valueOf e) \p ->
    transformState ctx (\state -> state { currentPrediction = updateProbability p state.currentPrediction })

correctnessChanged :: Handler
correctnessChanged ctx e =
  for_ (valueOf e) \c ->
    transformState ctx (\state -> state { currentPrediction = updateCorrectness c state.currentPrediction })

valueOf :: Event -> Either (NonEmptyList ForeignError) String
valueOf e = runExcept do
  target <- readProp "target" (toForeign e)
  value <- readProp "value" target
  readString value

login :: Array ReactElement
login =
          [ D.h3' [ D.text "Calibrate me!" ]
          , D.button [ P.onClick loginWithGoogle ]
                     [ D.text "Login with google" ]
          ]

select :: forall a props. (Show a) => ReactThis props State -> String -> Array a -> Handler -> ReactElement
select ctx current as h = D.select [ P.onChange (h ctx), P.value current ] $ map (option <<< show) as
  where
    option :: String -> ReactElement
    option a = D.option [ P.value a ] [ D.text a ]

cards :: forall props. ReactThis props State -> Prediction -> Array Prediction -> Array ReactElement
cards ctx (Prediction currentPrediction) predictions =
          [ D.h3' [ D.text "Calibrate me!" ]
          , D.button [ P.onClick logout ]
                     [ D.text "Logout" ]
          , D.div' [ D.input [ P.onChange (inputChanged ctx), P.value currentPrediction.name ] []
                   , select ctx (show currentPrediction.probability) [50, 70, 90] probabilityChanged
                   , select ctx (show currentPrediction.correct) [Unknown, Correct, Incorrect] correctnessChanged
                   , D.button [ P.onClick (addNewItem ctx) ] [ D.text "add" ] ]
          , D.text "predictions"
          , D.div' $ map (prediction ctx) predictions
          ]

prediction :: forall props. ReactThis props State -> Prediction -> ReactElement
prediction ctx (Prediction p) = D.div' [ D.text "prediction", D.text p.name, D.text $ show p.probability, D.text $ show p.correct ]

app :: forall props. ReactClass props
app = createClass $ spec'' initialState didMount \ctx -> do
  { loggedIn, predictions, currentPrediction } <- readState ctx
  pure $
    D.div [ P.className "container" ]
      if loggedIn then cards ctx currentPrediction predictions else login

main :: forall e. Eff (console :: CONSOLE, dom :: DOM, firebase :: FIREBASE | e) Unit
main = void do
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
