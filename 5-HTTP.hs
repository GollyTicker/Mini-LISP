{-# language OverloadedStrings #-}
import Web.Firefly
import qualified Data.Text as T
import Data.Maybe
import System.IO

main :: IO ()
main = do
  p <- getPort
  putStrLn $ "Running at port " ++ show p
  run p app

app :: App ()
app = do
  route "/health" healthHandler
  route "/MiniLISP/file" fileHandler

getPort :: IO Int
getPort = (\x -> read x :: Int) <$> readFile "5-HTTP-port.txt"

healthHandler :: Handler T.Text
healthHandler = return "healthy"

fileHandler :: Handler T.Text
fileHandler = return "TODO"
