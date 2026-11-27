module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> 
    case runStateError m s of
      Left err -> Left err
      Right (x :!: s') -> runStateError (f x) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw err = StateError (\_ -> Left err)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor var = StateError (\s ->
    case M.lookup var s of
      Nothing -> Left UndefVar
      Just n -> Right (n :!: s))
  -- Cambia el valor de una variable
  update var i = StateError (\s -> Right (() :!: M.insert var i s))

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval com = 
  case runStateError (stepCommStar com) initEnv of
    Left err -> Left err
    Right (_ :!: s) -> Right s

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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
stepComm (Repeat exp com) =
  return (Seq com (IfThenElse exp Skip (Repeat exp com)))

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
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
  if n2 == 0
    then throw DivByZero
    else return (n1 `div` n2)

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
  n1 <- evalExp e1
  n2 <- evalExp e2
  return (n1 < n2)
evalExp (Gt e1 e2) = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  return (n1 > n2)
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

