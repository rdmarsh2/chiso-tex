{-- 
Copyright 2013 Robert Marsh <rdmarsh2@gmail.com>
Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0
--}       


module Main where
import CHIso.Syntax
import CHIso.Rules
import CHIso.Checker
import CHIso.Parser
import qualified Data.ByteString as BS
import Args
import System.IO
import System.Exit
import System.Environment
import Text.Parsec
import Control.Monad.Reader
import Control.Monad.Error

main = do
  prog <- getProgName
  args <- getArgs
  let eProgMode = parseArgs args
  case eProgMode of
    Left err -> do hPrint stderr err
                   hPutStr stderr $ usage prog
                   exitFailure
    Right pMode -> do
      inStr <- BS.readFile $ inFile pMode
      let outDumper = 
              case outFile pMode of
                Nothing -> putStr
                Just file -> writeFile file
      let lMode = Args.labels pMode
      case everything inStr lMode $ inFile pMode of
        Left err -> hPutStr stderr err
        Right res -> outDumper res

everything :: BS.ByteString -> Mode -> FilePath -> Either String String
everything str mode file = do
  (ctx, t) <- case runP proof () file str of
    Left err -> Left $ show err
    Right res -> Right res
  deriv <- runReader (runErrorT $ check t) ctx
  return $ render deriv mode False

