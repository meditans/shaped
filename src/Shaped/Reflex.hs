{-# LANGUAGE DataKinds, ExplicitForAll, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TypeApplications             #-}
{-# LANGUAGE TypeFamilies, TypeOperators                                    #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Shaped.Reflex where

import Reflex
import Reflex.Dom
import Servant.Reflex
import Data.Text
import Generics.SOP
import qualified Data.Map as Map
import Data.Monoid

import Shaped
import Data.Functor.Const
import Data.Functor.Identity

-- | A formlet is a way of constructing an input widget for only a part of the
-- form (For example, only for the user password). The two parameters that are
-- passed are a generic event to force the update of the field, and the error to
-- be displayed (as the validation is done elsewhere).
newtype Formlet t m a = Formlet
  { unFormlet :: Event t () -> Dynamic t (Maybe Text) -> m (Dynamic t a) }

-- | An endpoint is a reflex function to query an endpoint. It takes as
-- arguments the input (in the same format the servant-reflex library takes it),
-- and an event to trigger the api request. It returns an event with a nested
-- either value: the first either is to track if the communication with the
-- server has gone well, the second one to discriminate between an error value
-- for the form or a validated input.
type Endpoint t m a s = Dynamic t (Either Text a)
                     -> Event t ()
                     -> m (Event t (ReqResult (Either (s (Const (Maybe Text))) a)))

-- | This is the function that should be called to construct a form for a
-- record. It takes as input a Shaped formlet, a Shaped validation, title and
-- attributes for the button, and the endpoint to which it should address the
-- request for the server. It returns a dynamic value representing the state of
-- the form in general, but also an event tracking the server response after a
-- request. Internally, the function creates the widgets for input, runs the
-- client-side validations, controls the communication via the endpoint.
form :: ( Shaped a s , Code a ~ c, SListI2 c, c ~ '[c1]
        , MonadWidget t m
        , Code (s (Formlet t m))              ~ Map2 (Formlet t m) c
        , Code (s (Const (Maybe Text)))             ~ Map2 (Const (Maybe Text)) c
        , Code (s (Either Text))                    ~ Map2 (Either Text) c
        , Code (s Identity)                         ~ Map2 Identity c
        , Code (s (Validation (Either Text)))       ~ Map2 (Validation (Either Text)) c
        , Code (s (Event t :.: Const (Maybe Text))) ~ Map2 (Event t :.: Const (Maybe Text)) c
        , Generic a
        , Generic (s (Event t :.: Const (Maybe Text)))
        , Generic (s (Formlet t m))
        , Generic (s (Const (Maybe Text)))
        , Generic (s (Either Text))
        , Generic (s (Validation (Either Text)))
        , Generic (s Identity))
     => s (Formlet t m)
     -> s (Validation (Either Text))
     -> Text -> Map.Map AttributeName Text
     -> Endpoint t m a s
     -> m ( Dynamic t (Either (s (Const (Maybe Text))) a)
          , Event t (Either Text (Either (s (Const (Maybe Text))) a)) )
form shapedWidget clientVal buttonTitle btnConfig endpoint = mdo
  postBuild <- getPostBuild
  -- Here I read a tentative user from the created interface
  rawUser <- createInterface send (splitShaped errorEvent) shapedWidget
  -- Here I define the button. This could probably be mixed with the button code
  send <- (formButton buttonTitle btnConfig) send (() <$ serverResponse)
  -- This part does the server request and parses back the response without
  -- depending on the types in servant-reflex
  serverResponse <- let query = either (const $ Left "Please fill correctly the fields above") Right <$> validationResult
                    in (fmap . fmap) parseReqResult (endpoint query send)
  let
    -- Validation result is the rawUser ran through the validation
    validationResult = transfGen . flip validateRecord clientVal <$> rawUser
    validationErrorComponent = either id (const nullError) <$> validationResult
    -- Error event is the sum of the event from the form and that of the server
    errorEvent = leftmost [ updated validationErrorComponent
                          , tagPromptlyDyn validationErrorComponent postBuild
                          , formErrorFromServer ]
    -- Here we retrieve only the error from the server events
    formErrorFromServer = fst . fanEither . snd . fanEither $ serverResponse
  -- In the end, I return both the dynamic containing the event and the raw
  -- event signal from the server. Probably the type could be changed slightly here.
  -- display validationResult
  return (validationResult, serverResponse)

-- | This function is only concerned with the creation of the visual widget. The
-- first argument forces the error, the second argument is the event response
-- from the server with an error, the third is the formlet per se. The functions
-- return a dynamic of a (non validated) record. This is a function meant to be
-- used internally.
createInterface :: forall t m a s c c1 .
  ( Shaped a s, Code a ~ c, SListI2 c, c ~ '[c1]
  , MonadWidget t m
  , Code (s (Formlet t m))              ~ Map2 (Formlet t m) c
  , Code (s (Event t :.: Const (Maybe Text))) ~ Map2 (Event t :.: Const (Maybe Text)) c
  , Generic (s (Formlet t m))
  , Generic (s (Event t :.: Const (Maybe Text)))
  , Generic a)
  => Event t ()
  -> s (Event t :.: Const (Maybe Text))
  -> s (Formlet t m)
  -> m (Dynamic t a)
createInterface e shapedError shapedFormlet = unComp . fmap to . hsequence $ hzipWith (subFun e) a b
  where
    a :: POP (Event t :.: Const (Maybe Text)) c
    a = singleSOPtoPOP . fromSOPI $ from shapedError
    b :: SOP (Formlet t m) c
    b = fromSOPI $ from shapedFormlet

-- | This function passes the actual arguments to the formlet. It's meant to be
-- used internally in createInterface
subFun :: MonadWidget t m => Event t () -> (Event t :.: Const (Maybe Text)) a -> Formlet t m a -> (m :.: Dynamic t) a
subFun e (Comp a) (Formlet f) = Comp $ do
  let eventWithoutConst = getConst <$> a
  dynamicError <- holdDyn Nothing eventWithoutConst
  f e dynamicError

-- | This function builds a button, which is disabled during the server requests
-- (so that the user can't repeatedly submit before the server sent the previous
-- response).
formButton :: DomBuilder t m => Text -> Map.Map AttributeName Text -> Event t () -> Event t () -> m (Event t ())
formButton buttonTitle initialAttr disable enable = divClass "form-group" $ do
  (e, _) <- element "button" conf (text buttonTitle)
  return (domEvent Click e)
  where
    conf = def & elementConfig_initialAttributes .~ initialAttr
               & elementConfig_modifyAttributes  .~ mergeWith (\_ b -> b)
                   [ const disableAttr <$> disable
                   , const enableAttr <$> enable ]
    disableAttr = fmap Just initialAttr  <> "disabled" =: Just "true"
    enableAttr  = fmap Just initialAttr  <> "disabled" =: Nothing

-- Either move this temporary in shaped with the intent of reporting that
-- upstream, or define a synonym. Discuss this on the generics-sop tracker!
instance (Applicative f, Applicative g) => Applicative (f :.: g) where
    pure x = Comp (pure (pure x))
    Comp f <*> Comp x = Comp ((<*>) <$> f <*> x)

--------------------------------------------------------------------------------
-- | Parse the response from the API, to not expose the users to the types in
-- servant-reflex
parseReqResult :: ReqResult a -> Either Text a
parseReqResult (ResponseSuccess a _) = Right a
parseReqResult (ResponseFailure t _) = Left t
parseReqResult (RequestFailure s)    = Left s

---------------------------------------------------------------------------------

-- I begin understanding this as belonging to a simple sum, not SOP or POP
-- | Create a blank error for a generic data type.
nullError :: forall a s c c1.
  ( Shaped a s, Code a ~ c, c ~ '[c1], SListI2 c
  , Code (s (Const (Maybe Text))) ~ Map2 (Const (Maybe Text)) c
  , Generic (s (Const (Maybe Text))))
  => s (Const (Maybe Text))
nullError = to . toSOPI
          -- . id @(SOP (Const (Maybe Text)) c)
          . singlePOPtoSOP
          . id @(POP (Const (Maybe Text)) c)
          $ hpure (Const Nothing)
