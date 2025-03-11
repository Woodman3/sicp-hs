{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Eval(
    eval,
    Env(..),
    IError(..),
    Intrp(..)
)where
import Token(Atom(..), SExp(..))

import qualified Data.Map as Map
import Control.Monad.Except(ExceptT, MonadError, throwError)
import Control.Monad.State(State, MonadState, get, modify,put)

-- first frame is local frame, last frame is global frame
newtype Env = Env [ Map.Map String Atom ] deriving Show
data IError = EvalError String 
    | CondError String
    | ApplyError String
    | PrimitiveError String
    | ToIntError String
    | ToBoolError String
    | UnknownOperatorError String
    deriving Show
newtype Intrp a = Intrp { intrp :: ExceptT IError (State Env) a }
    deriving (Functor, Applicative, Monad, MonadError IError, MonadState Env)

eval :: SExp -> Intrp Atom
eval ( Leaf (Var v)) = lookUpVar v 
eval ( Leaf a ) = return a
eval ( Node (x:xs) ) =eval x >>= \case
    Lambda vars e -> do
        args <- listOfValues xs
        apply (Lambda vars e) args
    Op "if" -> evalIf xs
    Op "cond" -> evalCond xs
    Op "begin" -> last <$> mapM eval xs
    Op "define" -> case xs of
        [Leaf (Var v),e] -> do
            ev <- eval e
            insertVar v ev
            return ev --todo: return void
        [Node ( fn:args ),body] -> do
            vars <- mapM extractVar args
            fn_name <- extractVar fn
            insertVar fn_name $ Lambda vars body
            return $ Op fn_name --todo: return void
        _ -> throwError $ EvalError "invalid define expression"
    Op "set1" -> case xs of
        [Leaf (Var v),e] -> do
            ev <- eval e
            _ <- lookUpVar v
            insertVar v ev
            return ev --todo: return void
        _ -> throwError $ EvalError "invalid set1 expression"
    Op "lambda" -> case xs of
        [Node args,e] -> do
            vars <- mapM extractVar args
            return $ Lambda vars e
        _ -> throwError $ EvalError "invalid lambda expression"
    Op o -> do
        args <- listOfValues xs
        apply (Op o) args
    _ -> throwError $ EvalError "invalid expression"
    where
        extractVar :: SExp -> Intrp String
        extractVar (Leaf (Var v)) = return v
        extractVar _ = throwError $ EvalError "invalid lambda expression"

eval _ = throwError $ EvalError "invalid expression"

lookUpVar :: String -> Intrp Atom 
lookUpVar v = do
    Env env <- get
    case env of
        [] -> throwError $ EvalError "variable not found"
        (x:xs) -> case Map.lookup v x of
            Just a -> return a
            Nothing -> do
                put $ Env xs
                r <- lookUpVar v
                put $ Env (x:xs)
                return r

insertVar :: String -> Atom -> Intrp ()
insertVar v a = modify $ \(Env m) -> Env $ case m of
    [] -> [Map.singleton v a]
    (x:xs) -> (Map.insert v a x):xs

evalCond :: [SExp] -> Intrp Atom
evalCond [] = throwError $ CondError "no true or else clause" 
evalCond (x:xs)= case x of
    Node [Leaf (Op "else"),y] -> eval y
    Node [c,y] -> do
        cond <- eval c
        if toBool cond
            then eval y
            else evalCond xs
    _ -> throwError $ CondError "invalid clause"

evalIf :: [SExp] -> Intrp Atom
evalIf [c,t,e] = do
    cond <- eval c
    if toBool cond
        then eval t
        else eval e
evalIf _ = throwError $ EvalError "invalid if expression"

apply :: Atom -> [Atom] -> Intrp Atom
apply (Op o) args = return $ primitiveProcedure o args
apply (Lambda vars e) args = do
    let l1 = length vars
    let l2 = length args
    if l1 /= l2 
        then throwError $ ApplyError $ 
            "invalid number of arguments, expected " ++ show l1 ++ " but got " ++ show l2
    else do
        let closure = Map.fromList $ zip vars args
        (Env oldEnv) <- get
        let newEnv = Env $ closure:oldEnv
        put newEnv
        r <- eval e
        put $ Env oldEnv
        return r

apply _ _ = throwError $ UnknownOperatorError "unknown operator"

listOfValues :: [SExp] -> Intrp [Atom]
listOfValues = mapM eval

primitiveProcedure :: String -> [Atom] -> Atom
primitiveProcedure "+" args = Int $ foldr (+) 0 $ map (toInt) args -- (+) return just 0 in lisp
primitiveProcedure "-" args = Int $ foldr1 (-) $ map (toInt) args -- (-) return will error in lisp
primitiveProcedure "*" args = Int $ foldr (*) 1 $ map (toInt) args -- (*) return just 1 in lisp
primitiveProcedure "/" args = Int $ foldr1 div $ map (toInt) args -- (/) return will error in lisp
primitiveProcedure "<" args = Bool $  and $ zipWith (<) (map toInt args) (tail $ map toInt args) -- in scheme, (<) return #t (< 1) return #t
primitiveProcedure ">" args = Bool $ and $ zipWith (>) (map toInt args) (tail $ map toInt args)
primitiveProcedure "=" args  = Bool $ and $ zipWith (==) (map toInt args) (tail $ map toInt args)
primitiveProcedure "<=" args = Bool $  and $ zipWith (<=) (map toInt args) (tail $ map toInt args) -- in scheme, (<) return #t (< 1) return #t
primitiveProcedure ">=" args = Bool $ and $ zipWith (>=) (map toInt args) (tail $ map toInt args)
primitiveProcedure "and" args = Bool $ and $ map toBool args
primitiveProcedure "or" args = Bool $ or $ map toBool args
primitiveProcedure "not" [Bool b] = Bool $ not b
primitiveProcedure _ _ = error "unknown operator"


toInt :: Atom -> Int
toInt (Int i) = i
toInt _ = error "not an integer"

toBool :: Atom -> Bool
toBool (Bool b) = b
toBool _ = error "not a boolean"
