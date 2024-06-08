
module Interpreter where 

import Lexer

isValue :: Expr -> Bool
isValue BTrue = True
isValue BFalse = True
isValue (Num _) = True
isValue (Lam _ _ _) = True
isValue _ = False

subst :: String -> Expr -> Expr -> Expr
subst x n BFalse =  BFalse
subst x n BTrue  =  BTrue
subst x n (Num v) = Num v
subst x n (Var v) = if (v == x) then
                        n
                    else
                        (Var v)
subst x n (Lam v t b) = Lam v t(subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (If e1 e2 e3) = If(subst x n e1) (subst x n e2) (subst x n e3)
--aula dia 20
subst x n (Or e1 e2) = Or (subst x n e1) (subst x n e2)
subst x n (Sub e1 e2) = Sub (subst x n e1) (subst x n e2)
subst x n (Times e1 e2) = Times (subst x n e1) (subst x n e2)
-- subst x n e = error ("algo ficou para tras" ++ show e)

step :: Expr -> Expr  
step (App (Lam v t b) e) = if (isValue e) then 
                            subst v e b
                         else
                            App(Lam v t b) (step e)
step (App e1 e2) = App (step e1) e2
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = Add (Num n1) (step e2)
step (Add e1 e2) = Add (step e1) e2 
step (And BFalse e2) = BFalse 
step (And BTrue e2) = e2 
step (And e1 e2) = And (step e1) e2 
step (If BTrue e1 e2) = e1 
step (If BFalse e1 e2) = e2 
step (If e1 e2 e3) = If (step e1) e2 e3 

--aula dia 20
step (Or BFalse e2) = e2
step (Or BTrue e2) = BTrue
step (Or e1 e2) = Or (step e1) e2  

step (Sub (Num n1) (Num n2)) = Num (n1 - n2)
step (Sub (Num n1) e2) = Sub (Num n1) (step e2)
step (Sub e1 e2) = Sub (step e1) e2 

step (Times (Num n1) (Num n2)) = Num (n1 * n2)
step (Times (Num n1) e2) = Times (Num n1) (step e2)
step (Times e1 e2) = Times (step e1) e2


eval :: Expr -> Expr
eval e = if isValue e then
            e
        else
            eval (step e) 