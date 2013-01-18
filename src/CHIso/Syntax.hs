{-- 
Copyright 2013 Robert Marsh <rdmarsh2@gmail.com>
Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0
--}
module CHIso.Syntax where
import Data.Set

data Prop = PVar String
          | PAnd Prop Prop
          | POr Prop Prop
          | PImp Prop Prop 
          | PNeg Prop deriving (Eq, Show)

data Term = TVar String
          | TLam String Prop Term
          | TApp Term Term
          | TPair Term Term
          | TProjL Term
          | TProjR Term
          | TInL Prop Term
          | TInR Prop Term
          | TCase Term String Term String Term
          | TLet String Term Term
          | TNeg Term Term
          | TCont Term Term Prop
          | TNElim Term deriving Show

equiv :: Prop -> Prop -> Bool
equiv = (==)

unFree :: Term -> Term -> Set String
unFree t1 t2 = union (free t1) (free t2)

bindFree :: String -> Term -> Set String
bindFree s t = delete s $ free t

free :: Term -> Set String
free (TVar s) = singleton s
free (TLam s p t) = bindFree s t
free (TApp t1 t2) = unFree t1 t2
free (TPair t1 t2) = unFree t1 t2
free (TInL p t) = free t
free (TInR p t) = free t
free (TLet s t1 t2) = union (free t1) (bindFree s t2)
free (TCase t1 s1 t2 s2 t3) = unions [free t1, bindFree s1 t2, bindFree s2 t3]
free (TNeg t p) = free t
free (TCont t1 t2 p) = unFree t1 t2
free (TNElim t) = free t
