module Main where

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad
import Data.Maybe
import Network.Socket
import Network.Wai.Handler.Warp
import System.Directory
import System.Environment

import Application

getSocket :: IO Socket
getSocket = do
  env <- getEnvironment
  fromJust $ do
    (do
      unixPath <- lookup "UNIX_SOCKET" env
      Just $ do
        sock <- socket AF_UNIX Stream defaultProtocol
        dfe <- doesFileExist unixPath
        when dfe $ removeFile unixPath
        bind sock $ SockAddrUnix unixPath
        listen sock 5
        return sock)
    <|>
    (do
      let port = fromMaybe "0" $ lookup "PORT" env
      let mhost = lookup "HOST" env
      Just $ do
        addrs <- getAddrInfo
          (Just $ defaultHints { addrSocketType = Stream
                               , addrFlags = [AI_PASSIVE]})
          mhost $ Just port
        let addr = head $ reverse addrs 
        sock <- socket (addrFamily addr) (addrSocketType addr)
                  (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock $ max 2048 maxListenQueue
        return sock)

main :: IO ()
main = do
  bracket getSocket close $ \sock -> do
    getSocketName sock >>= \n -> putStrLn $ "Listening on " ++ (show n)
    app (runSettingsSocket defaultSettings sock)

