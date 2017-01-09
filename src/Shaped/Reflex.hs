{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Shaped.Reflex where

import Reflex
import Reflex.Dom
import Servant.Reflex
import Data.Text
import Generics.SOP

import Shaped
import Data.Functor.Const
import Data.Functor.Identity

newtype FormletSimple t m a = FormletSimple (Event t () -> Dynamic t (Maybe Text) -> m (Dynamic t a))

type Endpoint t m a s = Dynamic t (Either Text a)
                     -> Event t ()
                     -> m (Event t (ReqResult (Either (s (Const (Maybe Text))) a)))

-- What should I do with two validations here? Maybe not
-- class (Shaped a s, MonadWidget t m) => EndpointC t m s a where
--   endpoint :: Dynamic t (Either Text a)
--            -> Event t ()
--            -> m (Event t (ReqResult (Either (s (Const (Maybe Text))) a)))

-- A more comprehensive alternative to a form
form :: (Shaped a s, MonadWidget t m
        , Code (s (FormletSimple t m)) ~ Map2 (FormletSimple t m) c
        , Code a ~ c, SListI c, MonadWidget t m
        , Generic (s (FormletSimple t m))
        , All SListI c
        , c ~ '[c1]
        , Code (s (FormletSimple t m)) ~ Map2 (FormletSimple t m) c
        , Code (s (Const (Maybe Text))) ~ Map2 (Const (Maybe Text)) c
        , Code (s (Either Text)) ~ Map2 (Either Text) c
        , Code (s Identity) ~ Map2 Identity c
        , Code (s (Validation (Either Text))) ~ Map2 (Validation (Either Text)) c
        , Code (s (Event t :.: Const (Maybe Text))) ~ Map2 (Event t :.: Const (Maybe Text)) c
        , Generic (s (Event t :.: Const (Maybe Text)))
        , Generic (s (Const (Maybe Text)))
        , Generic (s (Either Text))
        , Generic (s (Validation (Either Text)))
        , Generic (s Identity)
        , Generic a)
     => s (FormletSimple t m)
     -> s (Validation (Either Text))
     -> Endpoint t m a s
     -> m ( Dynamic t (Either (s (Const (Maybe Text))) a)
          , Event t (Either Text (Either (s (Const (Maybe Text))) a)) )
form shapedWidget clientVal endpoint = mdo
  postBuild <- getPostBuild
  -- Here I read a tentative user from the created interface
  rawUser <- createInterface (splitShaped errorEvent) send shapedWidget
  -- Here I define the button. This could probably be mixed with the button code
  send <- _buttonElement send (() <$ serverResponse)
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

-- This is a generic function, just a way of zipping and sequencing the two parts
createInterface :: forall t m a s c c1 .
  ( Shaped a s, Code a ~ c, SListI c, MonadWidget t m
  , Generic (s (FormletSimple t m))
  , All SListI c
  , c ~ '[c1]
  , Code (s (FormletSimple t m)) ~ Map2 (FormletSimple t m) c
  , Code (s (Event t :.: Const (Maybe Text))) ~ Map2 (Event t :.: Const (Maybe Text)) c
  , Generic (s (Event t :.: Const (Maybe Text)))
  , Generic a)
  => s (Event t :.: Const (Maybe Text))
  -> Event t ()
  -> s (FormletSimple t m)
  -> m (Dynamic t a)
createInterface shapedError e shapedFormlet = unComp . fmap to . hsequence $ hzipWith (subFun e) a b
  where
    a :: POP (Event t :.: Const (Maybe Text)) c
    a = singleSOPtoPOP . fromSOPI $ from shapedError
    b :: SOP (FormletSimple t m) c
    b = fromSOPI $ from shapedFormlet

subFun :: MonadWidget t m => Event t () -> (Event t :.: Const (Maybe Text)) a -> FormletSimple t m a -> (m :.: Dynamic t) a
subFun e (Comp a) (FormletSimple f) = Comp $ do
  let eventWithoutConst = getConst <$> a
  dynamicError <- holdDyn Nothing eventWithoutConst
  f e dynamicError

-- Either move this temporary in shaped with the intent of reporting that
-- upstream, or define a synonym. Discuss this on the generics-sop tracker!
instance (Applicative f, Applicative g) => Applicative (f :.: g) where
    pure x = Comp (pure (pure x))
    Comp f <*> Comp x = Comp ((<*>) <$> f <*> x)

splitShaped :: forall a s c c1 f g .
  ( Functor f, Shaped a s, Code a ~ c, c ~ '[c1], SListI c1
  , Generic (s g)
  , Code (s g) ~ Map2 g c
  , Code (s (f :.: g)) ~ Map2 (f :.: g) c
  , Generic (s (f :.: g))
  )
  => f (s g) -> s (f :.: g)
splitShaped ev = to . toSOPI . nPtoSingleSOP $ hliftA2 funz duplicato prs
  where
    prs :: NP (Projection g c1) c1
    prs = projections
    duplicato :: NP (K (f (s g))) c1
    duplicato = hpure (K ev)
    funz :: forall a. K (f (s g)) a -> (K (NP g c1) -.-> g) a -> (f :.: g) a
    funz (K dup) (Fn p) = Comp $ ((p . K) . singleSOPtoNP . (id @(SOP g c)) . fromSOPI . (id @(SOP I (Map2 g c))) . from) <$> dup


--------------------------------------------------------------------------------
-- Parse the response from the API. This function could be in servant-reflex
-- (not in reflex-dom). In the meantime we'll keep it in shaped.
parseReqResult :: ReqResult a -> Either Text a
parseReqResult (ResponseSuccess a _) = Right a
parseReqResult (ResponseFailure t _) = Left t
parseReqResult (RequestFailure s)    = Left s

---------------------------------------------------------------------------------

-- I begin understanding this as belonging to a simple sum, not SOP or POP
nullError :: forall a s c c1.
  ( Shaped a s
  , Code a ~ c
  , c ~ '[c1]
  , Code (s (Const (Maybe Text))) ~ Map2 (Const (Maybe Text)) c
  , Generic (s (Const (Maybe Text)))
  , All SListI c
  ) => s (Const (Maybe Text))
nullError = to . toSOPI
          . (singlePOPtoSOP :: POP (Const (Maybe Text)) c -> SOP (Const (Maybe Text)) c)
          $ (hpure (Const Nothing) :: POP (Const (Maybe Text)) c)


