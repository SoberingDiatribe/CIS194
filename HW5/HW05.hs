{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Paths Variables ------------------------------------
dogJPG, dogOrigJPG, victimsJSON :: String
dogJPG = "clues\\dog.jpg"
dogOrigJPG = "clues\\dog-original.jpg"
victimsJSON = "clues\\victims.json"

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret new old = do
    newBS <- BS.readFile new
    oldBS <- BS.readFile old
    return $ BS.pack $ filter (/=0) $ BS.zipWith xor newBS oldBS

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key file = do
    fileBS <- BS.readFile $ (file ++ ".enc")
    let infKey = BS.pack $ cycle $ BS.unpack key
    BS.writeFile file $ BS.pack $ BS.zipWith xor fileBS infKey

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file = do
    fileBS <- BS.readFile file
    return $ decode fileBS

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victims transactions = do
    tidsMaybe <- parseFile victims :: IO (Maybe [TId])
    transMaybe <- parseFile transactions :: IO (Maybe [Transaction])
    return $ case (tidsMaybe, transMaybe) of
                (Nothing, _) -> Nothing
                (_, Nothing) -> Nothing
                (Just tids, Just trans) -> Just (filter (\x -> (tid x) `elem` tids) trans)

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow transactions = foldr updateMap Map.empty transactions

updateMap :: Transaction -> Map String Integer -> Map String Integer
updateMap transaction mapData = Map.insertWith (+) (to transaction) (amount transaction) (Map.insertWith (+) (from transaction) ((-1) * (amount transaction)) mapData)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal mapData = fst foldr (\a@(_, (maximum $ Map.elems mapData

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

