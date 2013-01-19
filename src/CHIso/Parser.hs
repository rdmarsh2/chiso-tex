{-- 
 - Copyright 2013 Robert Marsh <rdmarsh2@gmail.com>
 - Licensed under the Apache License, Version 2.0
 -     http://www.apache.org/licenses/LICENSE-2.0
 --}

module CHIso.Parser where
import Control.Monad.Identity
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import CHIso.Syntax
import Text.Parsec
import Text.Parsec.Token (GenLanguageDef (..))
import qualified Text.Parsec.Token as Tok

type Parser a = Parsec ByteString () a

term :: Parser Term
term =
  (try $ parens term)
  <|>
  (parens $ do
    t1 <- term 
    comma
    t2 <- term
    return $ TPair t1 t2)
  <|> 
  (do symbol "\\"
      (s, p, t) <- annotatedBinding
      return $ TLam s p t)
  <|>
  (do reserved "case"
      t <- term
      reserved "of"
      (s1, t1) <- binding
      symbol "|"
      (s2, t2) <- binding
      return $ TCase t s1 t1 s2 t2)
  <|>
  (do reserved "injl"
      t <- term
      reserved "at"
      p <- prop
      return $ TInL p t)
  <|>
  (do reserved "injr"
      t <- term
      reserved "at"
      p <- prop
      return $ TInR p t)
  <|>
  (do reserved "projl"
      fmap TProjL term)
  <|>
  (do reserved "projr"
      fmap TProjR term)
  <|>
  (do reserved "let"
      s <- ident
      symbol "="
      t1 <- term
      reserved "in"
      t2 <- term
      return $ TLet s t1 t2)
  <|>
  (do reserved "neg"
      t1 <- term
      symbol "><"
      t2 <- term
      return $ TNeg t1 t2)
  <|>
  (do reserved "cont"
      t1 <- term
      symbol "><"
      t2 <- term
      reserved "thus"
      p <- prop
      return $ TCont t1 t2 p)
  <|>
  (do reserved "dne"
      fmap TNElim term)
  <|>
  (fmap TVar ident) 

binding :: Parser (String, Term)
binding = do
  s <- ident
  symbol "."
  t <- term
  return (s, t)

annotatedBinding :: Parser (String, Prop, Term)
annotatedBinding = do
  s <- ident
  colon
  p <- prop
  symbol "."
  t <- term
  return (s, p, t)

prop :: Parser Prop
prop =
  (try $ do
    p1 <- simpleProp
    o <- op
    p2 <- prop
    return $ o p1 p2)
  <|>
  simpleProp

op :: Parser (Prop -> Prop -> Prop)
op = 
  (do symbol "&"
      return PAnd)
  <|>
  (do symbol "|"
      return POr)
  <|> 
  (do symbol "->"
      return PImp)

simpleProp :: Parser Prop
simpleProp =
  parens prop
  <|>
  fmap PVar ident
  <|>
  (do symbol "-"
      fmap PNeg prop)
  

proofTermDef :: GenLanguageDef ByteString () Identity
proofTermDef = LanguageDef {
  commentStart   = "(*",
  commentEnd     = "*)",
  commentLine    = "//",
  nestedComments = True,
  identStart = alphaNum,
  identLetter = alphaNum <|> char '_',
  opStart = oneOf "",
  opLetter = oneOf "",
  reservedNames = [],
  reservedOpNames = [],
  caseSensitive = True
  }

proofTermTokens = Tok.makeTokenParser proofTermDef

ident = Tok.identifier proofTermTokens
comma = do _ <- Tok.comma proofTermTokens; return ()
parens = Tok.parens proofTermTokens
symbol = Tok.symbol proofTermTokens
colon = Tok.colon proofTermTokens
reserved = Tok.reserved proofTermTokens

