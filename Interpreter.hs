module Interpreter where

import Parser 

data Ty = TBool
        | TNum 
        | TPair Ty Ty
        deriving Show

--------------------------------------------
step :: Expr -> Maybe Expr

step (Add (Num n1) (Num n2)) = Just (Num (n1 + n2))
step (Add (Num n) e2) = 
    case (step e2) of
        Just e2' -> Just (Add (Num n) e2')
        Nothing  -> Nothing
step (Add e1 e2) = 
    case (step e1) of 
        Just e1' -> Just (Add e1' e2)
        Nothing  -> Nothing

step (Mult (Num n1) (Num n2)) = Just (Num (n1 * n2))
step (Mult (Num n) e2) = 
    case (step e2) of
        Just e2' -> Just (Mult (Num n) e2')
        Nothing  -> Nothing
step (Mult e1 e2) = 
    case (step e1) of 
        Just e1' -> Just (Mult e1' e2)
        Nothing  -> Nothing


step (Pair (Num n1) e2) = case (step e2) of
                           Just e2' -> Just (Pair (Num n1) e2')
                           Nothing  -> Nothing
step (Pair BFalse e2) = case (step e2) of
                           Just e2' -> Just (Pair BFalse e2')
                           Nothing  -> Nothing
step (Pair BTrue e2) = case (step e2) of
                           Just e2' -> Just (Pair BTrue e2')
                           Nothing  -> Nothing
step (Pair e1 e2) =  case (step e1) of 
                        Just e1' -> Just(Pair e1' e2)
                        Nothing  -> Nothing        

step (First (Pair (Num e1) _)) = Just (Num e1)
step (First (Pair BTrue _)) = Just (BTrue)
step (First (Pair BFalse _)) = Just (BFalse)
step (First (Pair e1 _)) = case (step e1) of 
                        Just e1' -> Just (First e1')
                        Nothing  -> Nothing
step (First e1) = case (step e1) of 
                        Just e1' -> Just (First e1')
                        Nothing  -> Nothing

step (Second (Pair _ (Num e1))) = Just (Num e1)
step (Second (Pair _ BTrue)) = Just (BTrue)
step (Second (Pair _ BFalse)) = Just (BFalse)
step (Second (Pair _ e1)) = case (step e1) of 
                        Just e1' -> Just (Second e1')
                        Nothing  -> Nothing
step (Second e1) = case (step e1) of 
                        Just e1' -> Just (Second e1')
                        Nothing  -> Nothing

-- S-Or1
step (Or BTrue _) = Just BTrue
-- S-Or2
step (Or _ BTrue) = Just BTrue
-- S-Or3
step (Or BFalse BFalse) = Just BFalse
-- S-Or4
step (Or e1 e2) = 
    case (step e2) of 
        Just e2' -> Just (Or e1 e2')
        Nothing  -> Nothing

-- S-And1
step (And BTrue e2) = Just e2
-- S-And2
step (And BFalse e2) = Just BFalse
-- S-And3
step (And e1 e2) = 
    case (step e1) of 
        Just e1' -> Just (And e1' e2)
        Nothing  -> Nothing

-- S-if1
step (If BTrue e1 e2) = Just e1
-- S-if2
step (If BFalse e1 e2) = Just e2
-- S-if3
step (If e1 e2 e3) = case (step e1) of
                       Just e1' -> Just (If e1' e2 e3)
                       Nothing  -> Nothing
step e = Just e

----------------------------------------------------
eval :: Expr -> Maybe Expr
eval e = case (step e) of 
           Just e' -> if (e == e') then
                        Just e
                      else
                        eval e'
           _ -> error "Semantic error: erro avaliando expressão!" 

----------------------------------------------
typeof :: Expr -> Maybe Ty 
typeof BTrue = Just TBool
typeof BFalse = Just TBool

typeof (Num _) = Just TNum
typeof (Add e1 e2) = case (typeof e1) of
                       Just TNum -> case (typeof e2) of
                                      Just TNum -> Just TNum
                                      _         -> Nothing -- erro de tipo
                       _         -> Nothing -- erro de tipo
typeof (Mult e1 e2) = case (typeof e1) of
                       Just TNum -> case (typeof e2) of
                                      Just TNum -> Just TNum
                                      _         -> Nothing -- erro de tipo
                       _         -> Nothing -- erro de tipo
typeof (And e1 e2) = case (typeof e1, typeof e2) of 
                       (Just TBool, Just TBool) -> Just TBool
                       _                        -> Nothing -- erro de tipo
typeof (Or e1 e2) = case (typeof e1, typeof e2) of 
                       (Just TBool, Just TBool) -> Just TBool
                       _                        -> Nothing -- erro de tipo
typeof (If e1 e2 e3) = case (typeof e1) of 
                         Just TBool -> case (typeof e2, typeof e3) of 
                                         (Just TBool, Just TBool) -> Just TBool
                                         (Just TNum, Just TNum)   -> Just TNum
                                         _                        -> Nothing -- erro de tipo
                         _          -> Nothing -- erro de tipo

typeof (Pair e1 e2) = case (typeof e1) of
                        Just e1' -> case (typeof e2) of
                                        Just e2' -> Just (TPair e1' e2')

typeof (First e1) = case (typeof e1) of
                    Just (TPair e1' e2') -> Just e1'
                    _          -> Nothing -- erro de tipo

typeof (Second e1) = case (typeof e1) of
                    Just (TPair e1' e2') -> Just e2'
                    _          -> Nothing -- erro de tipo

----------------------------------------
typecheck :: Expr -> Expr
typecheck e = case (typeof e) of 
                Just _ -> e
                _ -> error "Type error: erro na verificação de tipos!"

------------------------------------------
main = getContents >>= print . eval . typecheck . parser . lexer 
