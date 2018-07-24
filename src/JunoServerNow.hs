{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           JunoCommon

import           Data.GraphViz                     (textLabel)
import           Data.GraphViz.Attributes.Complete (Attribute (Label, RankDir),
                                                    Label (StrLabel),
                                                    RankDir (FromLeft))
import           Data.GraphViz.HC.Util             (doDots)
import qualified Data.GraphViz.Types.Generalised   as G (DotGraph)
import           Data.GraphViz.Types.Monadic       (GraphID (Str), cluster,
                                                    digraph, edge, graphAttrs,
                                                    (-->))
import qualified Data.Text.Lazy                    as L (Text)
default (L.Text)

------------------------------------------------------------------------------

junoServerNow :: G.DotGraph L.Text
junoServerNow = digraph (Str "junoServerNow") $ do
    cluster (Str "Command.hsBox") $ do
        graphAttrs [Label (StrLabel "Command.hs")]
        junoEnv; runCommand

    cluster (Str "ReceiverEnvBox") $ do
        graphAttrs [Label (StrLabel "ReceiverEnv/MessageReceiver")]
        getMessages; getNewCommands; getNewEvidence; getRvAndRVRs; enqueue

    cluster (Str "RaftSpecBox") $ do
        graphAttrs [Label (StrLabel "RaftSpec")]
        applyLogEntry; sendMessage;
        dequeue; enqueueLater;

    cluster (Str "Sender.hsBox") $ do
        graphAttrs [Label (StrLabel "Sender.hs")]
        sendRPC; sendAppendEntries; sendAppendEntriesResponse;
        sendResults;

    cluster (Str "Handle.hsBox") $ do
        graphAttrs [Label (StrLabel "Handle.hs")]
        handleEvents; handleRPC; issueBatch

    cluster (Str "H.AppendEntriesResponse.hsBox") $ do
        graphAttrs [Label (StrLabel "H.AppendEntriesResponse.hs")]
        handleAlotOfAers; appendEntriesResponseH; updateCommitProofMap;

    cluster (Str "Runtime.Timer.hsBox") $ do
        graphAttrs [Label (StrLabel "Timer.hs")]
        electionTimer; heartbeatTimer;

    graphAttrs [RankDir FromLeft]
    applyFn;
    inboxWR; outboxWR; rvAndRvrWR; cmdInboxWR; aerInboxWR;
    zmqSocketPull; zmqSocketPush;
    eventWR;
    runCommand; applyFn;
    doCommit;
    electionTimeoutH; heartbeatTimeoutH;
    appendEntriesH; requestVoteH; requestVoteResponseH; commandH; revolutionH;

    -- Apps.Juno.Server main
    "runCommand" --> "junoEnv"
    "applyFn" --> "runCommand"

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
    "applyLogEntry" --> "applyFn"
    "sendMessage" --> "outboxWR"
    "eventWR" --> "dequeue"
    "enqueueLater" --> "eventWR"

    -- Juno.Consensus.Commit
    "doCommit" --> "applyLogEntry"

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
    "appendEntriesResponseH" --> "updateCommitProofMap"
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
    "handleRPC" --> "requestVoteH"
    "handleRPC" --> "requestVoteResponseH"
    "handleRPC" --> "commandH"
    "handleRPC" --> "revolutionH"

    "appendEntriesH" --> "sendAppendEntriesResponse"
    edge "requestVoteH" "sendRPC" [textLabel "RVR'"]
    "requestVoteResponseH" --> "sendAppendEntries"
    "requestVoteResponseH" --> "electionTimer"
    "requestVoteResponseH" --> "heartbeatTimer"
    edge "commandH" "sendRPC" [textLabel "RetransmitToLeader |\nSendCommandResponse"]
    edge "commandH" "updateCommitProofMap" [textLabel "CommitAndPropagate"] -- TODO: what propagate?
    -- Juno.Runtime.Timer (Now/OK)
    "electionTimer" --> "enqueueLater"
    "heartbeatTimer" --> "enqueueLater"

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoServerNow"   , junoServerNow)
            ]
