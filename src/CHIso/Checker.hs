{-- 
Copyright 2013 Robert Marsh <rdmarsh2@gmail.com>
Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0
--}       


module CHIso.Checker where
import CHIso.Syntax
import Data.List
import CHIso.Rules
import Control.Monad.Trans.Error
import Control.Monad.Reader

buildRule :: Prop -> String -> [Rule] -> ErrorT String (Reader [(String, Prop)]) Rule
buildRule p s rs = do
  g <- lift $ asks $ reverse . map snd
  return $ Rule g p s rs

-- just to pull the conclusino out of a Rule.
-- might be better to convert to a record.
conc :: Rule -> Prop
conc (Rule _ p _ _) = p

-- the meat of the operation.  Keeps the context in the Reader.
-- I decided returning just a Rule and using conc was less inelegant than
-- returning a pair.  It's not clear that was right.
check :: Term -> ErrorT String (Reader [(String, Prop)]) Rule
check (TVar s) = do
  p <- lift $ asks $ find (\(x, y) -> x == s)
  case p of
    Just (_, p') -> buildRule p' "Assumed" []
    Nothing -> throwError $ "Free term variable: " ++ s
check (TLam s p t) = do
  r <- checkBind s p t
  let p' = conc r
  buildRule (PImp p p') "ImpInt" [r]
check (TApp t1 t2) = do
  r1 <- check t1
  let p1 = conc r1
  (p, p') <- case p1 of
    PImp p p' -> return (p, p')
    _ -> fail "Term applied is not an implication"
  r2 <- check t2
  let p2 = conc r2
  if p == p2
    then buildRule p' "ImpElim" [r1, r2]
    else fail $ mismatch p p2
check (TPair t1 t2) = do 
  r1 <- check t1
  let p1 = conc r1
  r2 <- check t2
  let p2 = conc r2
  buildRule (PAnd p1 p2) "AndInt" [r1, r2]
check (TProjL t) = do
  r <- check t
  case conc r of
    PAnd p1 p2 -> buildRule p1 "AndElimL" [r]
    _ -> fail "Term projected does not prove a conjunction"
check (TProjR t) = do
  r <- check t
  case conc r of
    PAnd p1 p2 -> buildRule p2 "AndElimR" [r]
    _ -> fail "Term projected does not prove a conjunction"
check (TInL p2 t) = do
  r <- check t
  let p1 = conc r
  buildRule (POr p1 p2) "OrIntL" [r]
check (TInR p1 t) = do
  r <- check t
  let p2 = conc r
  buildRule (POr p1 p2) "OrIntR" [r]
check (TCase t s1 t1 s2 t2) = do
  r <- check t
  let p = conc r
  (p1, p2) <- case p of
    POr p1 p2 -> return (p1, p2)
    _ -> fail "Term cased on does not prove a disjunction"
  r1 <- checkBind s1 p1 t1
  r2 <- checkBind s2 p2 t2
  let q1 = conc r1
  let q2 = conc r2
  if equiv q1 q2
    then buildRule q1 "OrElim" [r, r1, r2]
    else fail $ mismatch q1 q2
check (TLet s t1 t2) = do
  r1 <- check t1
  r2 <- checkBind s (conc r1) t2
  buildRule (conc r2) "Lemma" [r1, r2]
check (TNeg t1 t2) = do
  r1 <- check t1
  r2 <- check t2
  (p1, q1) <- case conc r1 of
    PImp p1 q1 -> return (p1, q1)
    _ -> fail "First term in proof by negation does not prove an implication"
  (p2, q2) <- case conc r2 of
    PImp p2 (PNeg q2) -> return (p2, q2)
    _ -> fail "Second term in proof by negation does not prove an implication of a negative"
  equiv' p1 p2
  equiv' q1 q2
  buildRule (PNeg p1) "NegInt" [r1, r2]
check (TCont t1 t2 p) = do
  r1 <- check t1
  let p1 = conc r1
  r2 <- check t2
  p2 <- case conc r2 of
    PNeg q2 -> return q2
    _ -> fail "Second term in contradiction does not prove a negative"
  equiv' p1 p2
  buildRule p "Cont" [r1, r2]
check (TNElim t) = do
  r <- check t
  case conc r of
    PNeg (PNeg p) -> buildRule p "NegElim" [r]
    _ -> fail "Term in double negation elimination does not prove a double negative"

mismatch :: Prop -> Prop -> String
mismatch p1 p2 = "Type mismatch: \n\t" ++ (show p1) ++ "\n\t" ++ (show p2)

equiv' :: Prop -> Prop -> ErrorT String (Reader [(String, Prop)]) ()
equiv' p1 p2 = 
  if equiv p1 p2
    then return ()
    else fail $ mismatch p1 p2

checkBind :: String -> Prop -> Term -> ErrorT String (Reader [(String, Prop)]) Rule
checkBind s p t = local ((s, p) :) (check t)
