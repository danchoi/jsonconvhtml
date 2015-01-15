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
import System.Process (readProcess, readProcessWithExitCode)
import qualified Text.Regex.PCRE.Light as R
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T

opts = O.info (O.helper)
          (O.fullDesc 
            <> O.progDesc [s|Convert all HTML in JSON objects strings to plain text.
                    On STDIN provide an input stream of newline-separated JSON objects. |]
            <> O.header "jsonconvhtml"
            <> O.footer "See https://github.com/danchoi/jsonconvhtml for more information.")

main = do
  O.execParser opts
  x <- BL.getContents 
  let xs :: [Value]
      xs = decodeStream x
  -- transform JSON
  let bashFilter = io "elinks" ["-dump"] (Just isHtml)
  xs' <- mapM (runFilter bashFilter) xs
  mapM_ (L8.putStrLn . encode) xs'

io :: String -> [String] -> Maybe (Text -> Bool) -> Value -> IO Value
io prog args preFilter v = 
    case v of 
      String v' -> do
        if runPreFilter v'
        then do 
            res <- readProcess prog args (T.unpack v')
            return . String . T.pack $ res
        else return v -- no op
      _ -> return v -- no op
  where 
    runPreFilter v = 
      case preFilter of 
        Just preFilter' -> preFilter' v
        _ -> True

htmlRegex :: R.Regex
htmlRegex = R.compile (B.pack regex) []
  where regex :: String
        regex = "</(html|body|p|ul|a|h1|h2|h3|table) *>|<br */?>|<img *src"

isHtml :: Text -> Bool
isHtml x = case R.match htmlRegex (T.encodeUtf8 x) [] of
    Just _ -> True
    Nothing -> False



------------------------------------------------------------------------
data FilterEnv = FilterEnv { filterProg :: (Value -> IO Value) } 

runFilter :: (Value -> IO Value) -> Value -> IO Value
runFilter ioFilter v = 
    case v of 
      (Object v') -> 
          runReaderT (runFilter' v) (FilterEnv ioFilter)
      x -> error $ "Expected Object, but got " ++ show x


runFilter' :: Value -> ReaderT FilterEnv IO Value 
runFilter' v = do
    io' <- asks filterProg
    case v of 
       (String _) -> liftIO $ io' v
       Null -> return Null
       (Number _) -> return v
       (Bool _) -> return v
       (Array _) -> return v          -- no effect on Arrays
       (Object hm) -> do
         let pairs = HM.toList hm
         pairs' <- mapM (\(k,v) -> (,) <$> pure k <*> runFilter' v) pairs
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
