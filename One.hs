module One where

data Expr = Val Int | Add Expr Expr

eval :: Applicative m => Expr -> m Int
eval (Val n) = pure n
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
