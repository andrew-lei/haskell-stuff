{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import ChineseParse (parsePage)
import Data.ByteString.UTF8 (fromString)
import Data.ByteString.Char8 (hPutStr)
import System.IO (withFile, IOMode(..))
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (unpack)
import Control.Monad (mapM_)

getLetter :: Manager -> Char -> IO ()
getLetter man letter = do
  req <- parseRequest $ "https://en.wiktionary.org/wiki/Index:Mandarin_Pinyin/" ++ [letter]
  response <- httpLbs req man
  let parsed = (parsePage . unpack . decodeUtf8 . responseBody) response
  withFile "./output.txt" AppendMode $ \handle -> do
    hPutStr handle parsed

main :: IO ()
main = do
    let settings = managerSetProxy
            (proxyEnvironment Nothing)
            tlsManagerSettings
    man <- newManager settings
    withFile "./output.txt" WriteMode $ \handle -> do
      hPutStr handle ""
    mapM_ (getLetter man) ['a'..'z']
