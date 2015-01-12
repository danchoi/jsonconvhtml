{-# LANGUAGE OverloadedStrings, QuasiQuotes, BangPatterns, ScopedTypeVariables #-}
module Main where
import Data.Text (Text)
import Control.Applicative
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy as BL hiding (map, intersperse, zip, concat)
import qualified Data.ByteString.Lazy.Char8 as L8 
import System.Time
import System.Environment (getArgs)
import Data.Aeson
import Data.Monoid
import Data.List (intersperse, zip)
import qualified Data.Attoparsec.Text as AT
import Data.Attoparsec.Lazy as Atto hiding (Result)
import Data.Attoparsec.ByteString.Char8 (endOfLine, sepBy)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Scientific  (Scientific, floatingOrInteger)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as O
import Control.Monad (when)
import System.Exit
import Data.String.QQ 

data Options = Options { 
    filterProgram :: String
  , fieldsToFilter :: String
  , debugKeyPaths :: Bool
  } deriving Show

parseOpts :: O.Parser Options
parseOpts = Options 
  <$> O.argument O.str (O.metavar "FILTERPROG" <> O.help "Shell filter pipeline")
  <*> O.argument O.str (O.metavar "FIELDS" <> O.help "JSON keypath expressions")
  <*> O.switch (O.long "debug" <> O.help "Debug keypaths")

opts = O.info (O.helper <*> parseOpts)
          (O.fullDesc 
            <> O.progDesc [s|Filter JSON object fields through shell.
                    On STDIN provide an input stream of newline-separated JSON objects. |]
            <> O.header "jsonbf"
            <> O.footer "See https://github.com/danchoi/jsonbashfilter for more information.")

main = do
  Options filterProg expr debugKeyPaths <- O.execParser opts
  x <- BL.getContents 
  let xs :: [Value]
      xs = decodeStream x
      ks = parseKeyPath $ T.pack expr
      ks' :: [[Key]]
      ks' = [k | KeyPath k <- ks]
      hs :: [Text] -- header labels
      hs = map keyPathToHeader ks
  -- extract JSON

  when debugKeyPaths $ do
     Prelude.putStrLn $ "Key Paths: " ++ show ks
     exitSuccess
  Prelude.putStrLn "TEST"  
 
------------------------------------------------------------------------
-- decode JSON object stream

decodeStream :: (FromJSON a) => BL.ByteString -> [a]
decodeStream bs = case decodeWith json bs of
    (Just x, xs) | xs == mempty -> [x]
    (Just x, xs) -> x:(decodeStream xs)
    (Nothing, _) -> []

decodeWith :: (FromJSON a) => Parser Value -> BL.ByteString -> (Maybe a, BL.ByteString)
decodeWith p s =
    case Atto.parse p s of
      Atto.Done r v -> f v r
      Atto.Fail _ _ _ -> (Nothing, mempty)
  where f v' r = (\x -> case x of 
                      Success a -> (Just a, r)
                      _ -> (Nothing, r)) $ fromJSON v'

------------------------------------------------------------------------
-- JSON parsing and data extraction

data KeyPath = KeyPath [Key] deriving Show

data Key = Key Text | Index Int deriving (Eq, Show)

parseKeyPath :: Text -> [KeyPath]
parseKeyPath s = case AT.parseOnly pKeyPaths s of
    Left err -> error $ "Parse error " ++ err 
    Right res -> res

keyPathToHeader :: KeyPath -> Text
keyPathToHeader (KeyPath ks) = 
    mconcat $ intersperse "." $ [x | Key x <- ks] -- exclude Index keys

spaces = many1 AT.space

pKeyPaths :: AT.Parser [KeyPath]
pKeyPaths = pKeyPath `AT.sepBy` spaces

pKeyPath :: AT.Parser KeyPath
pKeyPath = KeyPath 
    <$> (AT.sepBy1 pKeyOrIndex (AT.takeWhile1 $ AT.inClass ".["))


pKeyOrIndex :: AT.Parser Key
pKeyOrIndex = pIndex <|> pKey

pKey = Key <$> AT.takeWhile1 (AT.notInClass " .[:")

pIndex = Index <$> AT.decimal <* AT.char ']'

------------------------------------------------------------------------

-- runFilterOnPaths :: (Value -> IO Value) -> [KeyPath] -> Value -> IO Value
-- runFilterOnPaths ioFilter ks v = mapM (\kp -> runFilterOnPath filterProg kp v) ks


data FilterEnv = FilterEnv { targetKeyPath :: [Key] }

runFilterOnPath :: (Value -> IO Value) -> [Key] -> Value -> ReaderT FilterEnv IO Value 
runFilterOnPath ioFilter k v = undefined
-- runFilterOnPath ioFilter k (String _) = 


{-
-- evaluates the a JS key path against a Value context to a leaf Value
evalKeyPath :: ArrayDelimiter -> [Key] -> Value -> Value
evalKeyPath d [] x@(String _) = x
evalKeyPath d [] x@Null = x
evalKeyPath d [] x@(Number _) = x
evalKeyPath d [] x@(Bool _) = x
evalKeyPath d [] x@(Object _) = x
evalKeyPath d [] x@(Array v) | V.null v = Null
evalKeyPath d [] x@(Array v) = 
          let vs = V.toList v
              xs = intersperse d $ map (evalToText d []) vs
          in String . mconcat $ xs
evalKeyPath d (Key key:ks) (Object s) = 
    case (HM.lookup key s) of
        Just x          -> evalKeyPath d ks x
        Nothing -> Null
evalKeyPath d (Index idx:ks) (Array v) = 
      let e = (V.!?) v idx
      in case e of 
        Just e' -> evalKeyPath d ks e'
        Nothing -> Null
-- traverse array elements with additional keys
evalKeyPath d ks@(Key key:_) (Array v) | V.null v = Null
evalKeyPath d ks@(Key key:_) (Array v) = 
      let vs = V.toList v
      in String . mconcat . intersperse d $ map (evalToText d ks) vs
evalKeyPath _ ((Index _):_) _ = Null
evalKeyPath _ _ _ = Null
-}

