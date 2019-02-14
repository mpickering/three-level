{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Three where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data Expr = Val Int | Add Expr Expr

data WithCode a = WithCode {
                    _val :: a
                    , _code :: TExpQ a
                    }

data ApplicativeDict m = ApplicativeDict
                          { _return :: (forall a . WithCode (a -> m a)),
                            _ap     :: (forall a b . WithCode (m (a -> b) -> m a -> m b))
                          }

data SynApplicative a where
  Return :: WithCode a -> SynApplicative a
  App  :: SynApplicative (a -> b) -> SynApplicative a -> SynApplicative b

liftT :: Lift a => a -> TExpQ a
liftT = unsafeTExpCoerce . lift

codePlus = [|| (+) ||]

elimExpr :: Expr -> TExpQ (SynApplicative Int)
elimExpr (Val n) = [|| Return (WithCode n (liftT n)) ||]
elimExpr (Add e1 e2) =
   [|| Return (WithCode (+) codePlus) `App` $$(elimExpr e1) `App` $$(elimExpr e2) ||]

elimApplicative :: SynApplicative a -> ApplicativeDict m -> TExpQ (m a)
elimApplicative (Return v) d@ApplicativeDict{..} = [|| $$(_code _return) $$(_code v) ||]
elimApplicative (App e1 e2) d@ApplicativeDict{..} = [|| $$(_code _ap) $$(elimApplicative e1 d) $$(elimApplicative e2 d) ||]


data Identity a = Identity a

idAp :: Identity (a -> b) -> Identity a -> Identity b
idAp (Identity f) (Identity a) = Identity (f a)

identityDict = ApplicativeDict{..}
  where
    _return = WithCode Identity [|| Identity ||]
    _ap = WithCode idAp [|| idAp ||]




