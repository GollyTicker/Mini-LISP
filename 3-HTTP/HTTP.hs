{-# language OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Command
import System.Exit
import System.IO
import System.Unix.Directory
import Control.Concurrent
import Control.Concurrent.MVar

import Web.Firefly
import qualified Network.Wai as W
import Network.HTTP.Types.URI

import Control.Monad
import Control.Monad.Trans
import Data.Maybe

main :: IO ()
main = do
  p <- getPort
  to <- getTimeout
  putStrLn $ "Running at port " ++ show p
  run p $ withLogger $ app to

app :: Int -> App ()
app timeout = do
  -- routes are already started in new threads, so
  -- blocking behavior in these threads is acceptable
  route "/health" healthHandler
  route "/MiniLISP/file" (fileHandler timeout)

getPort :: IO Int
getPort = (\x -> read x :: Int) <$> readFile "3-HTTP/port.txt"

getTimeout :: IO Int
getTimeout = (\x -> read x :: Int) <$> readFile "3-HTTP/timeout-ms.txt"

healthHandler :: Handler T.Text
healthHandler = return "healthy"

fileHandler :: Int -> Handler (T.Text,Status)
fileHandler timeout = do
  mtxt <- getQuery "value"
  case mtxt of
    Nothing -> return ("",badRequest400)
    Just txt -> do
      res <- liftIO $ processRequestConcurrently txt timeout
      return (res, ok200)

processRequestConcurrently :: T.Text -> Int -> IO T.Text
processRequestConcurrently txt timeout = do
  mres <- newEmptyMVar
  forkIO $ withTimeoutIO mres timeout (processRequest txt)
  readMVar mres

processRequest :: T.Text -> MVar T.Text -> IO ()
processRequest input mres = withTemporaryDirectory "tmp" $ \dir ->
  do  let inputf = (dir ++ "/input.lisp")
      TIO.writeFile inputf input
      (Exit c, Stdout out) <- command [] "bin/MiniLISP" ["-f", inputf]
      res <- case c of
        ExitSuccess -> return $ T.pack out
        ExitFailure i ->  do
          putStrLn ("MiniLISP: Error ["++show i++"]")
          return $ "Unexpected internal error with MiniLISP, sorry!"
      putMVar mres res

withTimeoutIO :: MVar T.Text -> Int -> (MVar T.Text -> IO ()) -> IO ()
withTimeoutIO mv to f = do
    workerID <- forkIO (f mv)
    threadDelayMilliSeconds to
    timedout <- tryPutMVar mv timedoutResp
    when timedout $ killThread workerID
  where
    timedoutResp = T.pack $ "Time Limit Exceeded. " ++ show to ++ "ms"
    threadDelayMilliSeconds = threadDelay . (1000*)

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
