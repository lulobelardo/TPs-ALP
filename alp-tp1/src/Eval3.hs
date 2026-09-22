module Eval3
  ( eval
  , State
  , Error
  )
where

import AST
import qualified Data.Map.Strict as M
import Data.Strict.Tuple
import Data.Maybe (fromJust)

-- Estado: mapa de variables a enteros
type State = (M.Map Variable Int, String)


-- Estado vaci­o
-- No hay variables definidas y la traza esta vacia
initState :: State
initState = (M.empty,"")

-- Buscar variable
lookfor :: Variable -> State -> Either Error Int
lookfor v (m,s) = maybe (Left UndefVar) Right (M.lookup v m)

-- Actualizar variable
update :: Variable -> Int -> State -> State
update v e (m,traza) = (M.insert v e m,traza)

-- Agrega una nueva parte a la traza
addTrace :: String -> State -> State
addTrace str (m,s) = (m, s ++ str)

-- evalExp evalúa una expresión en un determinado estado
-- Devuelve: Left Error,  si ocurrió un error
--           Right (valor :!: estado), si la expresión pudo evaluarse

evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = Right (n :!: s)
evalExp (Var v) s = do
    val <- lookfor v s
    return (val :!: s)
evalExp (UMinus e) s = do
    (v :!: s1) <- evalExp e s
    return ((-v) :!: s1)
evalExp (VarInc v) s = do
    val <- lookfor v s
    let newVal = val + 1
        s1 = update v newVal s
    return (newVal :!: s1)
evalExp (VarDec v) s = do
    val <- lookfor v s
    let newVal = val - 1
        s1 = update v newVal s
    return (newVal :!: s1)
evalExp (Plus e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    return ((v1 + v2) :!: s2)
evalExp (Minus e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    return ((v1 - v2) :!: s2)
evalExp (Times e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    return ((v1 * v2) :!: s2)
evalExp (Div e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    if v2 == 0 then Left DivByZero else return ((v1 `div` v2) :!: s2)
evalExp BTrue s = Right (True :!: s)
evalExp BFalse s = Right (False :!: s)
evalExp (Lt e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    return ((v1 < v2) :!: s2)
evalExp (Gt e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    return ((v1 > v2) :!: s2)
evalExp (Eq e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    return ((v1 == v2) :!: s2)
evalExp (NEq e1 e2) s = do
    (v1 :!: s1) <- evalExp e1 s
    (v2 :!: s2) <- evalExp e2 s1
    return ((v1 /= v2) :!: s2)
evalExp (And b1 b2) s = do
    (v1 :!: s1) <- evalExp b1 s
    (v2 :!: s2) <- evalExp b2 s1
    return ((v1 && v2) :!: s2)
evalExp (Or b1 b2) s = do
    (v1 :!: s1) <- evalExp b1 s
    (v2 :!: s2) <- evalExp b2 s1
    return ((v1 || v2) :!: s2)
evalExp (Not b) s = do
    (v :!: s1) <- evalExp b s
    return ((not v) :!: s1)


-- stepComm ejecuta un paso de un comando sobre un estado,
-- produciendo el comando resultante y el nuevo estado,
-- o un error si la ejecución no es posible.
stepComm :: Comm -> State -> Either Error (Pair Comm State)

stepComm Skip s = Right (Skip :!: s)

stepComm (Let v e) s = do
    (val :!: s1) <- evalExp e s
    let s2 = update v val s1
    let s3 = addTrace ("Let " ++ v ++ " " ++ show val ++ " ") s2
    Right (Skip :!: s3)

stepComm (Seq Skip c2) s = Right (c2 :!: s)
stepComm (Seq c1 c2) s = do
    (c1' :!: s1) <- stepComm c1 s
    Right (Seq c1' c2 :!: s1)

stepComm (IfThenElse b c1 c2) s = do
    (cond :!: s1) <- evalExp b s
    if cond
       then Right (c1 :!: s1)
       else Right (c2 :!: s1)

stepComm (RepeatUntil c b) s = 
    Right (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)


stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'


-- Evaluador final
-- Comienza con el estado inicial: initState = (M.empty, "")
-- Se ejecuta el programa completamente mediante stepCommStar.
-- El resultado puede ser: Left Error o Right EstadoFinal

eval :: Comm -> Either Error State
eval p = stepCommStar p initState
