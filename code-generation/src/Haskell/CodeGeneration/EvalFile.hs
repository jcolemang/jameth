
module CodeGeneration.EvalFile where

import Scheme.Types
import Scheme.ParseTypes
import Interpreter.Evaluate
import Interpreter.Types
import Scheme.Parse

import Data.List
import Data.Maybe

newtype Valid =
  IsValid Bool
  deriving ( Show )

data Result
  = PE ParseError
  | EE EvalError
  | Val Value
  deriving ( Show )

data ResultLine
  = ResultLine String Result

instance Show ResultLine where
  show (ResultLine p (PE err)) =
    intercalate "," [ p
                    , "Nothing"
                    , show err
                    ]
  show (ResultLine p (EE err)) =
    intercalate "," [ p
                    , "Nothing"
                    , show err
                    ]
  show (ResultLine p (Val v)) =
    intercalate "," [ p
                    , displayValue v
                    , "Nothing"
                    ]

getResults :: String -> IO [ResultLine]
getResults content = do
  let contents = lines content
  mResults <- mapM execLine contents
  return $ do
    (l, mr) <- zip contents mResults
    case mr of
      Just r ->
        [ResultLine l r]
      Nothing ->
        []

writeResults :: [ResultLine] -> String
writeResults = intercalate "\n" . fmap show

execLine :: String -> IO (Maybe Result)
execLine line =
  case runParse line of
    Left err ->
      return . Just $ PE err
    Right prog -> do
      val <- execEval prog
      case val of
        Left err ->
          return . Just $ EE err
        Right (VProc _) ->
          return Nothing
        Right v ->
          return . Just $ Val v
