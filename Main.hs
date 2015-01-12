{-# LANGUAGE OverloadedStrings, QuasiQuotes, BangPatterns, ScopedTypeVariables #-}
module Main where
import Data.Text (Text)
import Control.Applicative
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy as BL hiding (map, intersperse, zip, concat, putStrLn)
import qualified Data.ByteString.Lazy.Char8 as L8 
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
import System.Process (readProcess)

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
  let (prog:args) = words filterProg
  x <- BL.getContents 
  let xs :: [Value]
      xs = decodeStream x
      ks = parseKeyPath $ T.pack expr
      ks' :: [[Key]]
      ks' = [k | KeyPath k <- ks]
  when debugKeyPaths $ do
     putStrLn $ "FilterProg: " ++ prog ++ show args
     Prelude.putStrLn $ "Key Paths: " ++ show ks
     exitSuccess
  -- transform JSON
  let bashFilter = io prog args
  xs' <- mapM (runFilterOnPaths bashFilter ks') xs
  mapM_ (L8.putStrLn . encode) xs'
  
io :: String -> [String] -> Value -> IO Value
io prog args v = 
  case v of 
    String v' -> do
      res <- readProcess prog args (T.unpack v')
      return . String . T.pack $ res
    _ -> return v -- no op

------------------------------------------------------------------------

runFilterOnPaths :: (Value -> IO Value) -> [[Key]] -> Value -> IO Value
runFilterOnPaths ioFilter ks v = 
  foldM 
    (\acc kp -> 
        case acc of 
          (Object acc') -> do
              v' <- runReaderT (runFilterOnPath [] acc) (FilterEnv kp ioFilter)
              case v' of
                (Object hm) -> return . Object $ HM.union hm acc'
                x -> error $ "Expected Object, but got " ++ show x
          x -> error $ "Expected Object, but got " ++ show x
    ) v ks

data FilterEnv = FilterEnv { targetKeyPath :: [Key]
                           , filterProg :: (Value -> IO Value)
                           } 

runFilterOnPath :: [Key] -> Value -> ReaderT FilterEnv IO Value 
runFilterOnPath k v = do
      -- liftIO $ putStrLn $ "runFilterPath " ++ show k 
      targetKeyPath' <- asks targetKeyPath
      bashFilter' <- asks filterProg
      if (k == targetKeyPath') 
      then liftIO $ bashFilter' v
      else go k v
  where 
    go :: [Key] -> Value -> ReaderT FilterEnv IO Value
    go _ x@(String _) = return x
    go _ Null = return Null
    go _ x@(Number _) = return x
    go _ x@(Bool _) = return x
    go _ x@(Array _) = return x          -- no effect on Arrays
    go ks x@(Object hm) = do
       let pairs = HM.toList hm
       pairs' <- mapM (\(k,v) -> (,) <$> pure k <*> runFilterOnPath (ks <> [Key k]) v) pairs
       return . Object . HM.fromList $ pairs'

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

