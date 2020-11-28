{-# language OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.IO

import Web.Firefly
import qualified Network.Wai as W
import Network.HTTP.Types.URI

import Control.Monad.Trans
import Data.Maybe

main :: IO ()
main = do
  p <- getPort
  putStrLn $ "Running at port " ++ show p
  run p (withLogger app)

app :: App ()
app = do
  route "/health" healthHandler
  route "/MiniLISP/file" fileHandler

getPort :: IO Int
getPort = (\x -> read x :: Int) <$> readFile "5-HTTP-port.txt"

healthHandler :: Handler T.Text
healthHandler = return "healthy"

fileHandler :: Handler (T.Text,Status)
fileHandler = do
  mtxt <- getQuery "value"
  case mtxt of
    Nothing -> return ("",badRequest400)
    Just txt -> do
      liftIO $ TIO.putStrLn $ "Process: " `mappend` txt
      return (txt, ok200)

withLogger :: App () -> App ()
withLogger = addMiddleware before after
  where
    before = do
      path <- getPath
      method <- getMethod
      liftIO . TIO.putStrLn $ "INFO: REQUEST [" <> method <> "] " <> path
      waiRequest

    after resp = do
      liftIO $ TIO.putStrLn $ "INFO: RESPONSE " `mappend` (T.pack $ show (W.responseStatus resp))
      return resp
