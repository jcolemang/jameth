
module CodeGeneration.EvalFile
  ( execFile )
where

import Scheme.ParseTypes
import Interpreter.Evaluate
import Interpreter.Types
import Scheme.Parse

import Prelude hiding ( mapM
                      , map
                      , filter
                      , unlines
                      , concat
                      )
import Data.List hiding ( map
                        , filter
                        , unlines
                        , concat
                        )
import Data.Text ( unpack
                 , pack
                 )
import Data.ByteString.Char8 ( ByteString
                             )
import Data.Conduit.Combinators ( sourceFile
                                , sinkFile
                                , concat
                                , linesUnbounded
                                , mapM
                                , mapE
                                , map
                                , filterE
                                , unlines
                                )
import Data.Conduit ( (.|)
                    , ($$)
                    , Source
                    , Conduit
                    )
import Data.Conduit.Text ( encodeUtf8
                         , decodeUtf8Lenient
                         )
import Data.Conduit.List ( chunksOf
                         )
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import qualified Control.Monad.Parallel as P ( mapM )

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
                    , "0"
                    , show err
                    ]
  show (ResultLine p (EE _)) =
    intercalate "," [ p
                    , "0"
                    , "1"
                    ]
  show (ResultLine _ (Val (VProc _))) = [  ]
  show (ResultLine p (Val v)) =
    intercalate "," [ p
                    , displayValue v
                    , "0"
                    ]


isAClosure :: Result -> Bool
isAClosure val =
  case val of
    (Val (VProc _)) -> True
    _ -> False

getFile :: MonadResource m => FilePath -> Source m String
getFile file =
  sourceFile file
  .| decodeUtf8Lenient
  .| linesUnbounded
  .| map unpack

execLines :: MonadResource m => Int -> Conduit String m ResultLine
execLines chunkSize =
  let execLineTuple codes = liftIO $ flip P.mapM codes $ \code -> do
        val <- execLine code
        return (code, val)
  in
    chunksOf chunkSize
    .| mapM execLineTuple
    .| filterE (not . isAClosure . snd)
    .| mapE (uncurry ResultLine)
    .| concat

pipeline :: MonadResource m => Int -> Conduit String m ByteString
pipeline groupSize =
  execLines groupSize
  .| map show
  .| unlines
  .| map pack
  .| encodeUtf8

execFile :: Int -> String -> String -> IO ()
execFile groupSize sourceF sinkF =
  runResourceT $
  getFile sourceF
  .| pipeline groupSize
  $$ sinkFile sinkF

execLine :: String -> IO Result
execLine line =
  case runParse line of
    Left err ->
      return $ PE err
    Right prog -> do
      val <- execEval prog
      case val of
        Left err ->
          return $ EE err
        Right eVals ->
          case last eVals of
            v ->
              return $ Val v
