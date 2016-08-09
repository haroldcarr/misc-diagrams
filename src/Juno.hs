{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Data.GraphViz                          (filled, shape, style,
                                                         textLabel, GraphvizCommand(TwoPi))
import           Data.GraphViz.Attributes.Colors.Brewer (BrewerColor (BC),
                                                         BrewerName (Pastel2),
                                                         BrewerScheme (BScheme))
import           Data.GraphViz.Attributes.Complete      (Attribute (Color, FixedSize, Height, Label, RankDir, Width),
                                                         Color (RGB), ColorList (..),
                                                         Label (StrLabel),
                                                         NodeSize (SetNodeSize),
                                                         Number (Int),
                                                         RankDir (FromLeft), Shape (Circle, BoxShape, DiamondShape, DoubleCircle),
                                                         toColor, toColorList)
import           Data.GraphViz.HC.Util                  (doDots, uCircle', uRectangle)
import           Data.GraphViz.HC.DiagramsTH            (mk)
import qualified Data.GraphViz.Types.Generalised        as G (DotGraph)
import           Data.GraphViz.Types.Monadic            (Dot,
                                                         GraphID (Str, Num),
                                                         cluster, digraph, edge,
                                                         graphAttrs, node,
                                                         (-->))
import qualified Data.Text                              as T (Text)
import           Data.Text.Lazy                         as L (Text)
import           Data.Word                              (Word8)
default (T.Text)

function      :: n -> Text -> Dot n
function       = uRectangle []

dataStructure :: n -> Text -> Dot n
dataStructure  = uCircle'

------------------------------------------------------------------------------
-- SERVER

mk "dataStructure"
   [ -- Apps.Juno.Server main
     ("toFromCommands","toFrom\nCommands")
   , ("commandMVarMap","Command\nMVarMap")

     -- App.Juno.Command
   , ("junoEnv", "JunoEnv")
     -- Juno.Spec.Simple runJuno
   , ("inboxWR","inboxWR")
   , ("cmdInboxWR", "cmdInboxWR")
   , ("aerInboxWR", "aerInboxWR")
   , ("rvAndRvrWR","rvAndRvrWR")
   , ("outboxWR","outboxWR")
   , ("eventWR","eventWR")
     -- RaftSpec: Juno.Spec.Simple simpleRaftSpec
   , ("cmdStatusMap", "cmd\nStatusMap")
   ]

mk "function"
   [ -- Apps.Juno.Server main
     ("applyFn","applyFn")

     -- App.Juno.Command
   , ("runCommand", "runCommand")

     -- Juno.Spec.Simple runJuno
   , ("pubMetric", "pubMetric")

     -- Juno.Runtime.Api.ApiServer
   , ("runApiServer","runApiServer")
   , ("apiEnv", "ApiEnv")

     -- Juno.Messaging.ZMQ runMsgServer
   , ("zmqSocketPull","zmqSocketPull")
   , ("zmqSocketPush","zmqSocketPush")

     -- ?
   , ("updateCmdMapFn", "updateCmdMapFn")

     -- ReceiverEnv : Juno.Runtime.MessageReceiver
   , ("getMessages", "getMessages")
   , ("getNewCommands", "getNewCommands")
   , ("getNewEvidence", "getNewEvidence")
   , ("getRvAndRVRs", "getRvAndRVRs")
   , ("enqueue", "enqueue")

     -- RaftSpec: Juno.Spec.Simple simpleRaftSpec
   , ("applyLogEntry", "applyLogEntry")
   , ("sendMessage", "sendMessage(s)")
   , ("publishMetric", "publishMetric")
   , ("enqueueMultiple", "enqueueMultiple")
   , ("enqueueLater", "enqueueLater")
   , ("dequeue", "dequeue")
   , ("updateCmdMap", "updateCmdMap")
   , ("dequeueFromApi", "dequeueFromApi")

     -- Juno.Consensus.Commit
   , ("doCommit", "doCommit")

     -- Juno.Runtime.Sender
   , ("sendDummyCollector", "*")
   , ("sendRPC", "sendRPC")
   , ("sendAppendEntries", "send(All)AppendEntries")
   , ("sendAppendEntriesResponse", "send(All)AppendEntriesResponse")

     -- Juno.Runtime.MessageReceiver
   , ("messageReceiver", "messageReceiver")

     -- Juno.Consensus.Handle
   , ("handleEvents", "handleEvents")
   , ("handleRPC", "handleRPC")
   , ("issueBatch", "issueBatch")
   -- Juno.Consensus.Handle.AppendEntriesResponse ...
   , ("handleAlotOfAers", "handleAlotOfAers")
   , ("appendEntriesResponseH", "handle")
   , ("updateCommitProofMap", "updateCommitProofMap")

   , ("electionTimeoutH", "electionTimeoutH")
   , ("heartbeatTimeoutH", "heartbeatTimeoutH")
   , ("appendEntriesH", "appendEntriesH")
   , ("requestVoteH", "requestVoteH")
   , ("requestVoteResponseH", "requestVoteResponseH")
   , ("commandH", "commandH")
   , ("revolutionH", "revolutionH")

     -- Juno.Runtime.Timer
   , ("electionTimer", "electionTimer")
   , ("heartbeatTimer", "heartbeatTimer")
   ]

junoServer :: G.DotGraph L.Text
junoServer = digraph (Str "junoServer") $ do
    cluster (Str "Command.hsBox") $ do
        graphAttrs [Label (StrLabel "Command.hs")]
        junoEnv; runCommand

    cluster (Str "ReceiverEnvBox") $ do
        graphAttrs [Label (StrLabel "ReceiverEnv")]
        getMessages; getNewCommands; getNewEvidence; getRvAndRVRs; enqueue

    cluster (Str "RaftSpecBox") $ do
        graphAttrs [Label (StrLabel "RaftSpec")]
        applyLogEntry; sendMessage;
        publishMetric; enqueueMultiple; enqueueLater; dequeue; updateCmdMap;
        cmdStatusMap; dequeueFromApi;

    cluster (Str "Sender.hsBox") $ do
        graphAttrs [Label (StrLabel "Sender.hs")]
        sendDummyCollector; sendRPC; sendAppendEntries; sendAppendEntriesResponse;

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
    toFromCommands;
    eventWR;
    runCommand; applyFn;
    commandMVarMap;
    runApiServer; apiEnv;
    messageReceiver
    pubMetric; updateCmdMapFn;
    doCommit;
    electionTimeoutH; heartbeatTimeoutH;
    appendEntriesH; requestVoteH; requestVoteResponseH; commandH; revolutionH;

    -- Apps.Juno.Server main
    "runCommand" --> "junoEnv"
    "applyFn" --> "runCommand"

    -- Juno.Spec.Simple runJuno

    -- Juno.Runtime.Api.ApiServer
    "commandMVarMap" --> "runApiServer"
    "runApiServer"   --> "toFromCommands"
    "apiEnv" --> "runApiServer"

    -- Juno.Messaging.ZMQ runMsgServer
    edge "zmqSocketPull"    "rvAndRvrWR" [textLabel "RV | RVR"]
    edge "zmqSocketPull"    "cmdInboxWR" [textLabel "CMD | CMDB"]
    edge "zmqSocketPull"    "aerInboxWR" [textLabel "AER"]
    edge "zmqSocketPull"    "inboxWR"    [textLabel "otherwise"]

    edge "outboxWR"       "zmqSocketPush" [textLabel "send rolodex"]

    -- ReceiverEnv : Juno.Runtime.MessageReceiver
    "inboxWR" --> "getMessages"
    "cmdInboxWR" --> "getNewCommands"
    "aerInboxWR" --> "getNewEvidence"
    "rvAndRvrWR" --> "getRvAndRVRs"
    "enqueue" --> "eventWR"

     -- RaftSpec: Juno.Spec.Simple simpleRaftSpec
    "applyLogEntry" --> "applyFn"
    "sendMessage" --> "outboxWR"
    "publishMetric" --> "pubMetric"
    "enqueueMultiple" --> "eventWR"
    "enqueueLater" --> "eventWR"
    "eventWR" --> "dequeue"
    "updateCmdMapFn" --> "updateCmdMap"
    "commandMVarMap" --> "cmdStatusMap"
    "toFromCommands" --> "dequeueFromApi"

    -- Juno.Consensus.Commit
    "doCommit" --> "applyLogEntry"

    -- Juno.Runtime.Sender
    "sendRPC"  --> "sendDummyCollector"
    "sendAppendEntries" --> "sendDummyCollector"
    "sendAppendEntriesResponse" --> "sendDummyCollector"
    "sendDummyCollector" --> "sendMessage"

    -- Juno.Runtime.MessageReceiver
    "getMessages" --> "messageReceiver"
    "getNewCommands" --> "messageReceiver"
    "getNewEvidence" --> "messageReceiver"
    "getRvAndRVRs" --> "messageReceiver"
    "messageReceiver" --> "enqueue"

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

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoServer", junoServer)
            ]
