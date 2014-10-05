{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.List hiding (insert)

parse :: String
      -> [LogMessage]
parse log_str = (map parseMessage) . lines $ log_str

parseMessage :: String
             -> LogMessage
parseMessage [] = Unknown "Empty String"
parseMessage (x:xs)
  | x == 'I'  = LogMessage Info (read (tokens!!0)) (unwords . drop 1 $ tokens)
  | x == 'E'  = LogMessage (Error (read (tokens!!0) :: Int)) (read (tokens!!1)) (unwords . drop 2 $ tokens)
  | x == 'W'  = LogMessage Warning (read (tokens!!0)) (unwords . drop 1 $ tokens)
  | otherwise = Unknown (x:xs)
  where tokens = words xs

insert :: MessageTree
       -> LogMessage
       -> MessageTree
insert tree (Unknown _) = tree
insert Leaf logmsg = Node Leaf logmsg Leaf
insert (Node left node_logmsg right) logmsg
  | tmstmp >= node_tmstmp = Node (insert left logmsg) node_logmsg right
  | tmstmp <  node_tmstmp = Node left node_logmsg (insert right logmsg)
  where (LogMessage _ tmstmp _)      = logmsg
        (LogMessage _ node_tmstmp _) = node_logmsg

build :: [LogMessage]
      -> MessageTree
build = foldl' insert Leaf

inOrder :: MessageTree
        -> [LogMessage]
inOrder Leaf                          = []
inOrder (Node left node_logmsg right) = inOrder right ++ node_logmsg : inOrder left

whatWentWrong :: [LogMessage]
              -> [String]
whatWentWrong = (map message) . (filter filter_pred)
  where filter_pred (LogMessage(Error severity) _ _) = severity > 50
        filter_pred _ = False
        message (LogMessage _ _ msg_str) = msg_str

testBuild = do
             logs <- testParse parse 10 "error.log"
             putStrLn $ show $ build logs

testInOrder = do
               logs <- testParse parse 10 "error.log"
               putStrLn $ show $ inOrder $ build logs
