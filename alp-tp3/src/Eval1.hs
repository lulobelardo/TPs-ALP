module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let var exp) = do
  n <- evalExp exp
  update var n
  return Skip
stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = do
  c1' <- stepComm c1
  return (Seq c1' c2)
stepComm (IfThenElse exp c1 c2) = do
  cond <- evalExp exp
  if cond then return c1 else return c2
stepComm (Repeat exp com) = do
  return (Seq com (IfThenElse exp Skip (Repeat exp com)))

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const n) = return n
evalExp (Var var) = lookfor var
evalExp (UMinus exp) = do
  n <- evalExp exp
  return (-n)
evalExp (Plus e1 e2) = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  return (n1 + n2)
evalExp (Minus e1 e2) = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  return (n1 - n2)
evalExp (Times e1 e2) = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  return (n1 * n2)
evalExp (Div e1 e2) = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  return (n1 `div` n2)

evalExp (VarInc x) = do
  n <- lookfor x
  update x (n + 1)
  return n
evalExp (VarDec x) = do
  n <- lookfor x
  update x (n - 1)
  return n

evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt e1 e2) = do
  b1 <- evalExp e1
  b2 <- evalExp e2
  return (b1 < b2)
evalExp (Gt e1 e2) = do
  b1 <- evalExp e1
  b2 <- evalExp e2
  return (b1 > b2)
evalExp (And e1 e2) = do
  b1 <- evalExp e1
  b2 <- evalExp e2
  return (b1 && b2)
evalExp (Or e1 e2) = do
  b1 <- evalExp e1
  b2 <- evalExp e2
  return (b1 || b2)
evalExp (Not exp) = do
  b <- evalExp exp
  return (not b)
evalExp (Eq e1 e2) = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  return (n1 == n2)
evalExp (NEq e1 e2) = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  return (n1 /= n2)