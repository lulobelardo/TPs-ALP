module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-----------------------
-- conversion
-----------------------

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
-- Ej 1
conversion = conv []
  where
    conv :: [String] -> LamTerm -> Term
    conv sigma (LVar x) = 
      case elemIndex x sigma of
        Just i -> Bound i
        Nothing -> Free (Global x)
    conv sigma (LAbs x typ t) = Lam typ (conv (x : sigma) t)
    conv sigma (LApp t1 t2) = conv sigma t1 :@: conv sigma t2
    -- Ej 3
    conv sigma (LLet x t1 t2) = Let (conv sigma t1) (conv (x : sigma) t2)
    -- Ej 4
    conv _ LZero = Zero
    conv sigma (LSuc t) = Suc (conv sigma t)
    conv sigma (LRec t1 t2 t3) = Rec (conv sigma t1) (conv sigma t2) (conv sigma t3)


----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
-- Ej 3
sub i t (Let u v) = Let (sub i t u) (sub (i + 1) t v)
-- Ej 4
sub _ _ Zero                  = Zero
sub i t (Suc u)               = Suc (sub i t u)
sub i t (Rec u v w)           = Rec (sub i t u) (sub i t v) (sub i t w)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f
-- Ej 4
quote (VNum NZero) = Zero
quote (VNum (NSuc n)) = Suc (quote (VNum n))

-- evalúa un término en un entorno dado
eval :: NameEnv Value Type -> Term -> Value
-- Ej 2
eval _ (Lam t u) = VLam t u
eval e (Free n) = 
  case lookup n e of
    Just (v, _) -> v
    Nothing -> error("Variable global no encontrada: " ++ show n)
eval _ (Bound _) = error "Variable ligada sin sustituir en eval"
eval e (t1 :@: t2) = 
  case eval e t1 of
    VLam _ u ->
      let v2 = eval e t2
      in eval e (sub 0 (quote v2) u)
    _ -> error "Error de tipos"
-- Ej 3 
eval e (Let t1 t2) =
  let v1 = eval e t1
  in eval e (sub 0 (quote v1) t2)
-- Ej 4
eval _ Zero = VNum NZero
eval e (Suc t) = 
  case eval e t of
    VNum n -> VNum (NSuc n)
    _ -> error "Error de tipos"
eval e (Rec t1 t2 t3) = 
  case eval e t3 of
    VNum NZero -> eval e t1
    VNum (NSuc n) ->
      let t = quote (VNum n)
      in eval e ((t2 :@: Rec t1 t2 t) :@: t)
    _ -> error "Error de tipos"

----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
-- Ej 3
infer' c e (Let t1 t2) =
  infer' c e t1 >>= \tt1 ->
  infer' (tt1 : c) e t2
-- Ej 4
infer' _ _ Zero = ret NatT
infer' c e (Suc t) =
  infer' c e t >>= \tt ->
  if tt == NatT then ret NatT else matchError NatT tt
infer' c e (Rec t1 t2 t3) =
  infer' c e t1 >>= \tt1 ->
  infer' c e t2 >>= \tt2 ->
  let expectedT2 = FunT tt1 (FunT NatT tt1)
  in if tt2 /= expectedT2
       then matchError expectedT2 tt2
       else infer' c e t3 >>= \tt3 ->
         if tt3 == NatT then ret tt1 else matchError NatT tt3

-- Ej 6
infer' c e Nil = ret ListT
infer' c e (Cons t1 t2) =
  infer' c e t1 >>= \tt1 ->
  if tt1 /= NatT then matchError NatT tt1
  else infer' c e t2 >>= \tt2 ->
  if tt2 /= ListT then matchError ListT tt2
  else ret ListT

