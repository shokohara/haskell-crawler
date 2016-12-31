{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import qualified Configuration.Dotenv as Dotenv
import System.Environment
import Text.XML.Cursor
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Simple
import Data.Maybe as Maybe
import Text.XML.HXT.Core
import Network.HTTP.Conduit
import Text.XML.HXT.XPath.Arrows
import Text.Regex.Applicative
import Text.Regex
import qualified Network.URI as URI

containRegex :: Regex -> String -> Bool
containRegex regex str = matchRegex regex str /= Nothing

grep :: String -> [String] -> [String]
grep = filter . containRegex . mkRegex

main :: IO ()
main = do
  Dotenv.loadFile False ".env"
  readFile "easylist.txt" >>= putStrLn . show . length . lines
  f

g :: [String] -> String -> IO String
g w u = do
  putStrLn u
  response <- parseRequest u >>= httpLBS
  x <- runX $ (readString [withParseHTML yes, withWarnings no] $ L8.unpack $ getResponseBody response) >>> getXPathTrees "/html/body/section[1]/div/div/div[1]/div/div[2]/iframe/@src" //> getText
  putStrLn . show $ x
  return $ head $ x

k :: [String] -> String -> IO [String]
k w u = do
  putStrLn u
  response <- parseRequest u >>= httpLBS
  runX $ (readString [withParseHTML yes, withWarnings no] $ L8.unpack $ getResponseBody response) >>> getXPathTrees "//*[@id=\"svvideo_html5_api\"]/@src" //> getText

f :: IO ()
f = do
  whitelist <- lines <$> readFile "whitelist.txt"
  url <- lookupEnv "URL"
  dst <- lookupEnv "DST"
  request <- parseRequest $ fromJust url
  response <- httpLBS request
  nodes <- runX $ (readString [withParseHTML yes, withWarnings no] $ L8.unpack $ getResponseBody response) >>> getXPathTrees "/html//a/@href" //> getText
  mapM_ (putStrLn . id) (grep (head whitelist) nodes)
  mapM_ (putStrLn . show) <$> (mapM (g []) (take 1 $ h (fromJust url) nodes whitelist))
  mapM_ (putStrLn . show) ((mapM (g []) (take 1 $ h (fromJust url) nodes whitelist)) >>= (fmap (k [])))
  return ()

h :: String -> [String] -> [String] -> [String]
h url nodes whitelist = (fmap (\v-> (\x-> "http://" ++ x ++ v) . URI.uriRegName . fromJust . URI.uriAuthority . fromJust . URI.parseURI $ url) (grep (head whitelist) nodes))

