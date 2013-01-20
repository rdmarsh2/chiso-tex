{-- 
 - Copyright 2013 Robert Marsh <rdmarsh2@gmail.com>
 - Licensed under the Apache License, Version 2.0
 -      http://www.apache.org/licenses/LICENSE-2.0
 -
 - convenience functions for debugging with ghci
--}

module CHIso.TestScripts where
import CHIso.Syntax
import CHIso.Rules
import CHIso.Checker
import CHIso.Parser

import Control.Monad.Error
import Control.Monad.Reader

import Text.Parsec
import Data.ByteString

handCheck :: [(String, Prop)] -> Term -> Either String Rule
handCheck g t = flip runReader g $runErrorT $ check t

handParse :: ByteString -> Either String Term
handParse s = case runP term () "(unknown)" s of
  Left err -> Left $ show err
  Right ans -> Right ans

handRender :: Rule -> String
handRender r = render r MRgt False

handRun :: [(String, Prop)] -> ByteString -> Either String String
handRun g s =  fmap handRender $ do
  t <- handParse s
  handCheck g t

blat :: Either String String -> IO ()
blat e = Prelude.putStrLn $
  case e of
    Left s -> s 
    Right s -> s

emptyDoAll :: IO ()
emptyDoAll = do
  s <- Data.ByteString.getLine
  let res = handRun [] s
  blat res
