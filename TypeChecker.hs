module TypeChecker where
import Lexer

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty
typeof ctx BTrue = Just TBool
typeof ctx BFalse = Just TBool

typeof ctx (Num _) = Just TNum

typeof ctx (Var v) = lookup v ctx
typeof ctx (Lam v t1 b) = case typeof ((v,t1) : ctx) b of
                           Just t2 -> Just (TFun t1 t2)
                           _ -> Nothing 
typeof ctx (App e1 e2) = 
   case (typeof ctx e1, typeof ctx e2) of
      (Just(TFun t11 t12), Just t2) -> if t11 == t2 then
                                          Just t12
                                       else
                                          Nothing

typeof ctx (Add e1 e2) = case   (typeof ctx e1, typeof ctx e2) of
                            (Just TNum, Just TNum) -> Just TNum 
                            _ -> Nothing 

typeof ctx (And e1 e2) = case   (typeof ctx e1, typeof ctx e2) of
                            (Just TBool, Just TBool) -> Just TBool
                            _ -> Nothing
                            
typeof ctx (If e1 e2 e3) = case (typeof ctx e1) of
                            Just TBool -> case (typeof ctx e2, typeof ctx e3) of
                                               (Just t1, Just t2) -> if t1 == t2 then
                                                                            Just t1
                                                                     else
                                                                        Nothing
                                               _-> Nothing                        
                            _ -> Nothing

--aula dia 20
typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                           (Just TBool, Just TBool) -> Just TBool
                           _ -> Nothing

typeof ctx (Sub e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                           (Just TNum, Just TNum) -> Just TNum
                           _ -> Nothing                           

typeof ctx (Times e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                             (Just TNum, Just TNum) -> Just TNum
                             _ -> Nothing
                           
typecheck :: Expr -> Expr
typecheck  e = case typeof [] e of
                     Just _ -> e
                     Nothing -> error "Type error"