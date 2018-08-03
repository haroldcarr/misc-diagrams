{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Data.GraphViz                     (textLabel)
import           Data.GraphViz.Attributes.Complete (Attribute (Label, Pad, NodeSep, RankSep, RankDir),
                                                    Label (StrLabel),
                                                    DPoint (DVal),
                                                    RankDir (FromLeft))
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

junoServerNow :: G.DotGraph L.Text
junoServerNow = digraph (Str "junoServerNow") $ do
    cluster (Str "RaftStateBox") $ do
        graphAttrs [Label (StrLabel "RaftState")]
        logEntries; commitIndex; commitProof; replayMap; membershipState;
        recentAndTentativeStates;

    cluster (Str "Command.hsBox") $ do
        graphAttrs [Label (StrLabel "Command.hs")]
        appState; runOrCommitCommand

    cluster (Str "ReceiverEnvBox") $ do
        graphAttrs [Label (StrLabel "ReceiverEnv/MessageReceiver")]
        getMessages; getNewCommands; getNewEvidence; getRvAndRVRs; enqueue

    cluster (Str "RaftSpecBox1") $ do
        graphAttrs [Label (StrLabel "RaftSpec1")]
        applyLogEntry; sendMessage;

    cluster (Str "RaftSpecBox2") $ do
        graphAttrs [Label (StrLabel "RaftSpec2")]
        dequeue; enqueueLater;

    cluster (Str "Sender.hsBox") $ do
        graphAttrs [Label (StrLabel "Sender.hs")]
        sendRPC; sendAppendEntries; sendAppendEntriesResponse;
        sendResults;

    cluster (Str "Runtime.Timer.hsBox") $ do
        graphAttrs [Label (StrLabel "Timer.hs")]
        electionTimer; heartbeatTimer;

    cluster (Str "HandleBox") $ do
        graphAttrs [Label (StrLabel "Handle")]
        handleEvents; handleRPC; issueBatch
        doCommit; handleAlotOfAers; commitAndPropagateCollector;
        appendEntriesH; appendEntriesResponseH;
        commandH; commandResponseH; cqH; cqrH;
        electionTimeoutH; heartbeatTimeoutH;
        msdH; requestVoteH; requestVoteResponseH; revolutionH;

    graphAttrs [ RankDir FromLeft
               , Pad (DVal 0.5)
               , NodeSep 0.5
               , RankSep [1.0]
               ]

    -- Transport/ZMQ
    inboxWR; outboxWR; rvAndRvrWR; cmdInboxWR; aerInboxWR;
    zmqSocketPull; zmqSocketPush;

    eventWR;

    -- Apps.Juno.Server main
    "runOrCommitCommand" --> "appState"

    -- Juno.Spec.Simple runJuno

    -- Juno.Messaging.ZMQ runMsgServer (Now/OK)
    edge "zmqSocketPull"    "rvAndRvrWR" [textLabel "RV | RVR"]
    edge "zmqSocketPull"    "cmdInboxWR" [textLabel "CMD | CMDB"]
    edge "zmqSocketPull"    "aerInboxWR" [textLabel "AER"]
    edge "zmqSocketPull"    "inboxWR"    [textLabel "otherwise"]
    edge "outboxWR"      "zmqSocketPush" [textLabel "send rolodex"]

    -- ReceiverEnv : Juno.Runtime.MessageReceiver (Now/OK)
    "inboxWR" --> "getMessages"
    "cmdInboxWR" --> "getNewCommands"
    "aerInboxWR" --> "getNewEvidence"
    "rvAndRvrWR" --> "getRvAndRVRs"
    "enqueue" --> "eventWR"
    edge "getMessages"    "enqueue" [textLabel "ERPC"]
    edge "getNewCommands" "enqueue" [textLabel "ERPC"]
    edge "getNewEvidence" "enqueue" [textLabel "AERs"]
    edge "getRvAndRVRs"   "enqueue" [textLabel "ERPC"]

     -- RaftSpec: Juno.Spec.Simple simpleRaftSpec
    "applyLogEntry" --> "runOrCommitCommand"
    "sendMessage" --> "outboxWR"
    "eventWR" --> "dequeue"
    "enqueueLater" --> "eventWR"

    -- Juno.Consensus.Commit
    "doCommit" --> "commitProof"
    "doCommit" --> "commitIndex"
    edge "doCommit"  "sendResults" [textLabel "if Leader"]
    edge "doCommit"  "applyLogEntry" [textLabel "commit"]

    -- Juno.Runtime.Sender (Now/OK)
    "sendRPC"  --> "sendMessage"
    "sendAppendEntries" --> "sendRPC"
    "sendAppendEntriesResponse" --> "sendRPC"
    "sendResults" --> "sendRPC"

    -- Juno.Consensus.Handle
    "dequeue" --> "handleEvents"
    "handleEvents" --> "handleRPC"
    edge "handleEvents" "issueBatch" [textLabel "if Leader"] -- condition is really inside `issueBatch`
    "issueBatch" --> "doCommit"
    "issueBatch" --> "sendAppendEntries"
    "issueBatch" --> "sendAppendEntriesResponse"

    -- Juno.Consensus.Handle.AppendEntriesResponse
    "handleEvents" --> "handleAlotOfAers"
    "handleAlotOfAers" --> "appendEntriesResponseH"
    "appendEntriesResponseH" --> "commitProof"
    "appendEntriesResponseH" --> "doCommit"
    "appendEntriesResponseH" --> "electionTimer"

    -- Juno.Consensus.Handle.ElectionTimeout
    "handleEvents" --> "electionTimeoutH"
    edge "electionTimeoutH" "sendRPC" [textLabel "castLazyVote |\nsendRequestVote"]
    "electionTimeoutH" --> "electionTimer"

    -- Juno.Consensus.Handle.HeartbeatTimeout
    "handleEvents" --> "heartbeatTimeoutH"
    edge "heartbeatTimeoutH" "sendAppendEntries" [textLabel "IsLeader"]
    edge "heartbeatTimeoutH" "heartbeatTimer" [textLabel "IsLeader"]
    edge "heartbeatTimeoutH" "enqueue" [textLabel "NoFollers : ElectionTimeout"]

    "handleRPC" --> "appendEntriesH"
    "handleRPC" --> "appendEntriesResponseH"
    "handleRPC" --> "commandH"
    "handleRPC" --> "commandResponseH"
    "handleRPC" --> "cqH"
    "handleRPC" --> "cqrH"
    "handleRPC" --> "msdH"
    "handleRPC" --> "requestVoteH"
    "handleRPC" --> "requestVoteResponseH"
    "handleRPC" --> "revolutionH"

    "appendEntriesH" --> "sendAppendEntriesResponse"
    edge "requestVoteH" "sendRPC" [textLabel "RVR'"]
    "requestVoteResponseH" --> "sendAppendEntries"
    "requestVoteResponseH" --> "electionTimer"
    "requestVoteResponseH" --> "heartbeatTimer"

    -- Juno.Consensus.Handle.Command
    edge "commandH" "sendRPC" [textLabel "RetransmitToLeader |\nSendCommandResponse"]
    -- TODO: what propagate?
    edge "commandH" "commitAndPropagateCollector" [textLabel "CommitAndPropagate"]
    edge "commitAndPropagateCollector" "applyLogEntry" [textLabel "apply"]
    "commitAndPropagateCollector" --> "logEntries"
    "commitAndPropagateCollector" --> "recentAndTentativeStates"
    "commitAndPropagateCollector" --> "replayMap"
    "commitAndPropagateCollector" --> "commitProof"

    -- Juno.Runtime.Timer (Now/OK)
    "electionTimer" --> "enqueueLater"
    "heartbeatTimer" --> "enqueueLater"

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoServerNow"   , junoServerNow)
            ]
