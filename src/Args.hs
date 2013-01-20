{-- 
 - Copyright 2013 Robert Marsh <rdmarsh2@gmail.com>
 - Licensed under the Apache License, Version 2.0
 -     http://www.apache.org/licenses/LICENSE-2.0
 --}

module Args where
import System.Console.GetOpt
import CHIso.Rules(Mode(..))

data ProgMode = ProgMode 
  { outFile :: Maybe FilePath
  , inFile :: FilePath
  , labels :: Mode
  }

defaultProgMode = ProgMode
  { outFile = Nothing
  , inFile = ""
  , labels = MRgt
  }

setLabels :: Mode -> ProgMode -> ProgMode
setLabels m pm = pm {labels = m}

setInFile :: FilePath -> ProgMode -> ProgMode
setInFile fp pm = pm {inFile = fp}

data ArgErrors = NoInputFile
               | MultipleInputfiles
               | GetOptError [String]

parseArgs :: [String] -> Either String ProgMode
parseArgs args = let
  (transforms, files, errors) = getOpt Permute argTable args
  in case errors of 
    [] -> case files of 
      [] -> Left "No input file specified"
      _ : _ : _ -> Left "Multiple input files specified"
      [file] -> Right $ foldr ($) (setInFile file defaultProgMode) transforms
    _ -> Left $ concat errors

argTable :: [OptDescr (ProgMode -> ProgMode)]
argTable = [
  Option ['o']["out"] (ReqArg outOptHandle "") "Sets the output file.  Defaults to printing on stdout.",
  Option [] ["label-left"] (NoArg (setLabels MLft)) "Typeset labels on the left instead of the right."
  ]

outOptHandle :: String -> ProgMode -> ProgMode
outOptHandle arg pmode =
  case arg of 
    "" -> pmode {outFile = Nothing}
    s -> pmode {outFile = Just s}

usage :: String -> String
usage prog = usageInfo prog argTable

