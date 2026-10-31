module PrettyPrinter
  ( printTerm  ,     -- pretty printer para terminos
    printType        -- pretty printer para tipos
  )
where

import  Common
import  Text.PrettyPrint.HughesPJ
import  Prelude hiding ((<>))

-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
-- Ej 3 
pp ii vs (Let t1 t2) =
  text "let "
    <> text (vs !! ii)
    <> text " = "
    <> pp ii vs t1
    <> text " in "
    <> pp (ii + 1) vs t2
-- Ej 4
pp _  _  Zero = text "0"
pp ii vs (Suc t) =
  text "suc " <> parensIf (isLamOnly t || isLet t || isRec t) (pp ii vs t)
pp ii vs (Rec t1 t2 t3) = sep
  [ text "R"
  , parensIf (not (isAtom t1)) (pp ii vs t1)
  , parensIf (not (isAtom t2)) (pp ii vs t2)
  , parensIf (isLamOnly t3 || isLet t3) (pp ii vs t3)
  ]

-- Ej 4
isAtom :: Term -> Bool
isAtom (Bound _) = True
isAtom (Free  _) = True
isAtom Zero      = True
isAtom _         = False

-- Ej 4
isLamOnly :: Term -> Bool
isLamOnly (Lam _ _) = True
isLamOnly _         = False

isLam :: Term -> Bool
isLam (Lam _ _) = True
-- Ej 3
isLam (Let _ _) = True
-- Ej 4
isLam (Rec _ _ _) = True
isLam (Suc _)     = True
isLam _         = False

-- Ej 3
isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _         = False

-- Ej 4
isRec :: Term -> Bool
isRec (Rec _ _ _) = True
isRec _           = False

-- Ej 4
isSuc :: Term -> Bool
isSuc (Suc _) = True
isSuc _       = False 

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
-- Ej 4
printType NatT   = text "Nat"
-- Ej 6
printType ListT = text "List Nat"



isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
-- Ej 3
fv (Let t1 t2) = fv t1 ++ fv t2
-- Ej 4
fv Zero               = []
fv (Suc t           ) = fv t
fv (Rec t1  t2  t3  ) = fv t1 ++ fv t2 ++ fv t3
-- Ej 6
fv Nil = []
fv (Cons t u) = fv t ++ fv u

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

