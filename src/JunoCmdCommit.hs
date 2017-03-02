{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           JunoCommon

import           Data.GraphViz                          (GraphvizCommand (TwoPi),
                                                         filled, shape, style,
                                                         textLabel)
import           Data.GraphViz.Attributes.Colors.Brewer (BrewerColor (BC),
                                                         BrewerName (Pastel2),
                                                         BrewerScheme (BScheme))
import           Data.GraphViz.Attributes.Complete      (Attribute (Color, Compound, FixedSize, Height, Label, RankDir, Width),
                                                         Color (RGB),
                                                         ColorList (..),
                                                         Label (StrLabel),
                                                         NodeSize (SetNodeSize),
                                                         Number (Int),
                                                         RankDir (FromLeft),
                                                         Shape (BoxShape, Circle, DiamondShape, DoubleCircle),
                                                         toColor, toColorList)
import           Data.GraphViz.HC.DiagramsTH            (mk)
import           Data.GraphViz.HC.Util                  (doDots, uCircle',
                                                         uRectangle)
import qualified Data.GraphViz.Types.Generalised        as G (DotGraph)
import           Data.GraphViz.Types.Monadic            (Dot,
                                                         GraphID (Num, Str),
                                                         cluster, digraph, edge,
                                                         graphAttrs, node,
                                                         (-->))
import qualified Data.Text.Lazy                         as L (Text)
import           Data.Word                              (Word8)
default (L.Text)

------------------------------------------------------------------------------

junoCmdCommit :: G.DotGraph L.Text
junoCmdCommit = digraph (Str "junoCmdCommit") $ do

    graphAttrs [ {- RankDir FromLeft, -} Compound True]

    client; follower;

    "client" --> "handleEvents"
    edge "follower" "handleEvents" [textLabel "AER"]
    edge "follower" "handleEventsF" [textLabel "AER"]

    cluster (Str "leaderId") $ do
        graphAttrs [Label (StrLabel "leader")]
        handleEvents; handleCommand;
        issueBatch; doCommit; applyLogEntries; applyCommand; apply;
        appendLogEntry; replayMap; logEntries; commitProof;
        sendResults; sendAppendEntries; sendAppendEntriesResponse;
        updateCommitIndex; checkCommitProof;
        appendEntriesResponseH; updateCommitProofMap;

    edge "handleEvents"         "handleCommand"  [textLabel "1.1"]
    edge "handleCommand"        "appendLogEntry" [textLabel "1"]
    edge "appendLogEntry"       "logEntries"     [textLabel "put"]
    edge "handleCommand"        "replayMap"      [textLabel "2\nput\nNothing"]
    edge "handleCommand"        "commitProof"    [textLabel "3\nAER"]

    edge "handleEvents"         "issueBatch" [textLabel "1.2/2.2"]
    edge "issueBatch"           "doCommit" [textLabel "1"]
    edge "doCommit"             "updateCommitIndex" [textLabel "1"]
    "updateCommitIndex" -->     "checkCommitProof"
    "checkCommitProof" -->      "commitProof"

    edge "doCommit"             "applyLogEntries" [textLabel "2\nwhen quorum"]
    edge "applyLogEntries"      "logEntries"      [textLabel "0\nget"]
    edge "applyLogEntries"      "applyCommand"    [textLabel "1"]
    edge "applyCommand"         "apply"           [textLabel "1"]
    edge "applyCommand"         "replayMap"       [textLabel "2\nput\nJust"]
    edge "applyLogEntries"      "sendResults"     [textLabel "3"]
    "sendResults" -->           "client"

    edge "issueBatch"           "sendAppendEntriesResponse" [textLabel "2"]
    edge "issueBatch"           "sendAppendEntries" [textLabel "3"]
    "sendAppendEntries" -->     "handleEventsF"
    "sendAppendEntries" -->     "follower"

    edge "handleEvents"         "appendEntriesResponseH" [textLabel "2.1"]
    edge "appendEntriesResponseH" "updateCommitProofMap" [textLabel "1"]
    "updateCommitProofMap" -->  "commitProof"
    edge "appendEntriesResponseH" "doCommit"             [textLabel "2"]

    cluster (Str "followerId") $ do
        graphAttrs [Label (StrLabel "follower")]
        handleEventsF; handleAppendEntries; appendLogEntries;
        addLogEntriesAt; updateLogHashesFromIndex; logEntriesF;
        updateCommitProofMapF; commitProofF;
        sendAppendEntriesResponseF;

    "handleEventsF" -->         "handleAppendEntries"
    edge "handleAppendEntries"  "appendLogEntries"           [textLabel "1"]
    "appendLogEntries" -->      "addLogEntriesAt"
    "addLogEntriesAt" -->       "updateLogHashesFromIndex"
    edge "handleAppendEntries"  "logEntriesF"                [textLabel "2\nCommit"]
    edge "handleAppendEntries"  "updateCommitProofMapF"      [textLabel "3\nCommit"]
    "updateCommitProofMapF" --> "commitProofF"
    edge "handleAppendEntries"  "sendAppendEntriesResponseF" [textLabel "4"]
    "sendAppendEntriesResponseF" --> "handleEvents"
    "sendAppendEntriesResponseF" --> "follower"

------------------------------------------------------------------------------
junoCmdCommit2 :: G.DotGraph L.Text
junoCmdCommit2 = digraph (Str "junoCmdCommit2") $ do

    graphAttrs [ {- RankDir FromLeft, -} Compound True]

    client; follower;

    "client" --> "handleEvents"
    edge "follower" "handleEvents" [textLabel "AER"]
    edge "follower" "handleEventsF" [textLabel "AER"]

    cluster (Str "leaderId") $ do
        graphAttrs [Label (StrLabel "leader")]
        handleEvents; handleCommand;
        issueBatch; doCommit; applyLogEntries; applyCommand; apply;
        appendLogEntry; replayMap; logEntries; commitProof;
        sendResults; sendAppendEntries; sendAppendEntriesResponse;
        updateCommitIndex; checkCommitProof;
        appendEntriesResponseH; updateCommitProofMap;

    edge "handleEvents"         "handleCommand"  [textLabel "1.1"]
    edge "handleCommand"        "apply"          [textLabel "1"]
    edge "handleCommand"        "appendLogEntry" [textLabel "2"]
    edge "appendLogEntry"       "logEntries"     [textLabel "put"]
    edge "handleCommand"        "replayMap"      [textLabel "3\nput\nNothing"]
    edge "handleCommand"        "commitProof"    [textLabel "4\nAER"]

    edge "handleEvents"         "issueBatch" [textLabel "1.2/2.2"]
    edge "issueBatch"           "doCommit" [textLabel "1"]
    edge "doCommit"             "updateCommitIndex" [textLabel "1"]
    "updateCommitIndex" -->     "checkCommitProof"
    "checkCommitProof" -->      "commitProof"

    edge "doCommit"             "applyLogEntries" [textLabel "2\nwhen quorum"]
    edge "applyLogEntries"      "logEntries"      [textLabel "0\nget"]
    edge "applyLogEntries"      "applyCommand"    [textLabel "1"]
    edge "applyCommand"         "replayMap"       [textLabel "2\nput\nJust"]
    edge "applyLogEntries"      "sendResults"     [textLabel "3"]
    "sendResults" -->           "client"

    edge "issueBatch"           "sendAppendEntriesResponse" [textLabel "2"]
    edge "issueBatch"           "sendAppendEntries" [textLabel "3"]
    "sendAppendEntries" -->     "handleEventsF"
    "sendAppendEntries" -->     "follower"

    edge "handleEvents"         "appendEntriesResponseH" [textLabel "2.1"]
    edge "appendEntriesResponseH" "updateCommitProofMap" [textLabel "1"]
    "updateCommitProofMap" -->  "commitProof"
    edge "appendEntriesResponseH" "doCommit"             [textLabel "2"]


    cluster (Str "followerId") $ do
        graphAttrs [Label (StrLabel "follower")]
        handleEventsF; handleAppendEntries; appendLogEntries;
        addLogEntriesAt; verifyLogHashesFromIndex; validResponses; applyF; logEntriesF;
        updateCommitProofMapF; commitProofF;
        sendAppendEntriesResponseF;

    "handleEventsF" -->         "handleAppendEntries"
    edge "handleAppendEntries"  "appendLogEntries"           [textLabel "1"]
    "appendLogEntries" -->      "addLogEntriesAt"
    "addLogEntriesAt" -->       "verifyLogHashesFromIndex"
    edge "handleAppendEntries"  "validResponses"             [textLabel "2\nCommit"]
    "validResponses" -->        "applyF"
    edge "handleAppendEntries"  "logEntriesF"                [textLabel "3\nCommit\n& valid"]
    edge "handleAppendEntries"  "updateCommitProofMapF"      [textLabel "4\nCommit\n& valid"]
    "updateCommitProofMapF" --> "commitProofF"
    edge "handleAppendEntries"  "sendAppendEntriesResponseF" [textLabel "5"]
    "sendAppendEntriesResponseF" --> "handleEvents"
    "sendAppendEntriesResponseF" --> "follower"

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoCmdCommit" , junoCmdCommit)
            , ("junoCmdCommit2", junoCmdCommit2)
            ]
