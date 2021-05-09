{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
-- import Debug.Trace (trace)

-- EXERCISE 1

parseErrorMessage :: String -> LogMessage
parseErrorMessage str = 
    LogMessage (Error (read severity)) (read timestamp) (unwords msg) where
    severity:timestamp:msg = words str

parseInfoMessage :: String -> LogMessage
parseInfoMessage str = 
    LogMessage Info (read timestamp) (unwords msg) where
    timestamp:msg = words str


parseMessage :: String -> LogMessage
-- parseMessage str | trace (show str) False = undefined
parseMessage ('E':rest) = parseErrorMessage rest
parseMessage ('I':rest) = parseInfoMessage rest
parseMessage invalid = Unknown invalid

parse :: String -> [LogMessage]
parse str =
    map parseMessage $ lines str

sampleLog :: [Char]
sampleLog =
    "I 6 Completed armadillo processing\n\
    \I 1 Nothing to report\n\
    \I 4 Everything normal\n\
    \I 11 Initiating self-destruct sequence\n\
    \E 70 3 Way too many pickles\n\
    \E 65 8 Bad pickle-flange interaction detected\n\
    \W 5 Flange is due for a check-up\n\
    \I 7 Out for lunch, back in two time steps\n\
    \E 20 2 Too many pickles\n\
    \I 9 Back from lunch\n\
    \E 99 10 Flange failed!"
    
expectedFromSampleLog :: [LogMessage]
expectedFromSampleLog =
    [ LogMessage Info 6 "Completed armadillo processing"
    , LogMessage Info 1 "Nothing to report"
    , LogMessage Info 4 "Everything normal"
    , LogMessage Info 11 "Initiating self-destruct sequence"
    , LogMessage (Error 70) 3 "Way too many pickles"
    , LogMessage (Error 65) 8 "Bad pickle-flange interaction detected"
    , Unknown "W 5 Flange is due for a check-up"
    , LogMessage Info 7 "Out for lunch, back in two time steps"
    , LogMessage (Error 20) 2 "Too many pickles"
    , LogMessage Info 9 "Back from lunch"
    , LogMessage (Error 99) 10 "Flange failed!"]

-- EXCERCISE 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert 
    msg@(LogMessage _ timeStampMsg _) 
    (Node treeLeft root@(LogMessage _ timeStampRoot _) treeRight) 
    = 
    if timeStampMsg < timeStampRoot then
        Node (insert msg treeLeft) root treeRight
    else
        Node treeLeft root (insert msg treeRight)
insert msg Leaf = Node Leaf msg Leaf

-- EXCERCISE 3

build :: [LogMessage] -> MessageTree
build logs =
    foldr insert Leaf logs

-- EXCERCISE 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) =
    inOrder left ++ [root] ++ inOrder right

-- EXCERCISE 5

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error severity) _ _) =
    severity > 50
isSevereError _ = False

getLogMessage :: LogMessage -> String
getLogMessage (LogMessage _ _ msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs =
    map getLogMessage $ filter isSevereError  $ inOrder (build msgs)

-- TESTS

runTests :: IO ()
runTests =
    putStrLn $ show 
        [ ("1", parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help")
        , ("2", parseMessage "I 29 la la la" == LogMessage Info 29 "la la la")
        , ("3", parseMessage "This is not in the right format" == Unknown "This is not in the right format")
        , ("parse", parse sampleLog == expectedFromSampleLog)
        ]

main :: IO ()
main = 
    runTests