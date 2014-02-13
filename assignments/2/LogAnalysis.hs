{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage     :: String -> LogMessage
parseMessage msg =
    case words msg of
      ("I":t:s) -> LogMessage Info (read t) (unwords s)
      ("W":t:s) -> LogMessage Warning (read t) (unwords s)
      ("E":i:t:s) -> LogMessage (Error (read i)) (read t) (unwords s)
      _ -> Unknown msg

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)


insert                  :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf         = Node Leaf msg Leaf
insert msg@(LogMessage _ t _) tree@(Node left msg'@(LogMessage _ t' _) right)
    | t == t' = tree
    | t < t' = Node (insert msg left) msg' right
    | t > t' = Node left msg' (insert msg right)


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)


whatWentWrong      :: [LogMessage] -> [String]
whatWentWrong msgs = map getBody $ filter getErrors $ inOrder (build msgs)
    where getErrors (LogMessage t _ _) = case t of
                                   Info -> False
                                   Warning -> False
                                   (Error n) -> n >= 50
          getBody (LogMessage _ _ b) = b