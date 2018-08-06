{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Data.GraphViz                     (textLabel)
import           Data.GraphViz.Attributes.Complete (Attribute (Label, RankDir),
                                                    Label (StrLabel),
                                                    RankDir (..))
import           Data.GraphViz.HC.Util             (doDots)
import qualified Data.GraphViz.Types.Generalised   as G (DotGraph)
import           Data.GraphViz.Types.Monadic       (GraphID (Str), cluster,
                                                    digraph, edge, graphAttrs,
                                                    (-->))
import qualified Data.Text.Lazy                    as L (Text)
------------------------------------------------------------------------------
import           Juno.JunoCommon
default (L.Text)
------------------------------------------------------------------------------

junoCommit :: G.DotGraph L.Text
junoCommit = digraph (Str "junoCommit") $ do
    cluster (Str "RaftStateBox") $ do
        graphAttrs [Label (StrLabel "RaftState")]
        commitIndex; commitProof; lastCommitTime; replayMap; lastCompleted;
    cluster (Str "RaftCommitHSBox1") $ do
        graphAttrs [Label (StrLabel "Raft Commit.hs")]
        cmtdoCommit; cmtupdateCommitIndex;
    cluster (Str "RaftCommitHSBox2") $ do
        graphAttrs [Label (StrLabel "Raft Commit.hs")]
        qciGTci; setcommitIndexToQCI; filterQCILTaerIndex; logCommitChange; getTime;
        completeCommittedLogEntries; getEntriesInLeftHalfOpenInterval; getAndSendResults; completeCommand;
        sendResults; commitLogEntry; sendRPCs;
    cluster (Str "PureCommitHSBox") $ do
        graphAttrs [Label (StrLabel "pure Commit.hs")]
        cmtcheckCommitProof; cmtfinalEvidence; filterEQCT; filterGCIandLEQMaLI; hashChecks; sortByDownIx;
        ccpRight;
    graphAttrs [RankDir FromLeft]
    -- PURE
    edge "cmtcheckCommitProof" "cmtfinalEvidence" [textLabel "nid->AER"]
    edge "cmtfinalEvidence"    "ccpRight" [textLabel "2; length FE >= quorumSize"]
    edge "cmtfinalEvidence"    "filterEQCT" [textLabel "1; AERs"]
    "filterEQCT"           --> "filterGCIandLEQMaLI"
    "filterGCIandLEQMaLI"  --> "hashChecks"
    "hashChecks"           --> "sortByDownIx"
    -------------------------
    -- Raft
    "cmtdoCommit"          --> "cmtupdateCommitIndex"
    "cmtupdateCommitIndex" --> "cmtcheckCommitProof"
    "ccpRight"             --> "qciGTci"
    "qciGTci"              --> "setcommitIndexToQCI"
    "setcommitIndexToQCI"  --> "commitIndex"
    "qciGTci"              --> "filterQCILTaerIndex"
    "filterQCILTaerIndex"  --> "commitProof"
    "qciGTci"              --> "logCommitChange"
    "logCommitChange"      --> "getTime"
    "getTime"              --> "lastCommitTime"
    "qciGTci"              --> "completeCommittedLogEntries"
    "completeCommittedLogEntries" --> "getEntriesInLeftHalfOpenInterval"
    edge "getEntriesInLeftHalfOpenInterval" "getAndSendResults" [textLabel "Seq LogEntry"]
    "getAndSendResults"           --> "completeCommand"
    "completeCommand"             --> "replayMap"
    edge "completeCommand"            "sendResults" [textLabel "(ClientId, resp); if Leader"]
    edge "getEntriesInLeftHalfOpenInterval" "commitLogEntry" [textLabel "Seq LogEntry"]
    "commitLogEntry"              --> "sendRPCs"
    "completeCommittedLogEntries" --> "lastCompleted"

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoCommit"   , junoCommit)
            ]
