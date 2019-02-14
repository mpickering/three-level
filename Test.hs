{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Test where

import Three



elim :: Identity Int
elim = $$(elimApplicative $$(elimExpr (Add (Val 1) (Val 2))) identityDict)
