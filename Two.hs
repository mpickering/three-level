{-# LANGUAGE TemplateHaskell #-}
module Two where

import Language.Haskell.TH

data Expr = Val Int | Add Expr Expr

eval :: Expr -> ExpQ
eval (Val n) = [| return n |]
eval (Add e1 e2) = [| (+) <$> $(eval e1) <*> $(eval e2) |]
