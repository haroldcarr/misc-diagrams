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
import           Data.GraphViz.HC.Util                  (doDots, uRectangle)
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

rectangle    :: n -> Text -> Dot n
rectangle     = uRectangle []

------------------------------------------------------------------------------
-- SERVER

mk "rectangle"
   [ -- Apps.Juno.Server main
     ("toFromCommands","toFromCommands")
   , ("commandMVarMap","CommandMVarMap")
   , ("applyFn","applyFn")

     -- App.Juno.Command
   , ("junoEnv", "JunoEnv")
   , ("runCommand", "runCommand")

     -- Juno.Spec.Simple runJuno
   , ("inboxWR","inboxWR")
   , ("cmdInboxWR", "cmdInboxWR")
   , ("aerInboxWR", "aerInboxWR")
   , ("rvAndRvrWR","rvAndRvrWR")
   , ("outboxWR","outboxWR")
   , ("eventWR","eventWR")
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
   , ("getMessageRS", "getMessage(s)")
   , ("getNewCommandsRS", "getNewCommands")
   , ("getNewEvidenceRS", "getNewEvidence")
   , ("getRvAndRVRsRS", "getRvAndRVRs")
   , ("publishMetric", "publishMetric")
   , ("enqueueRS", "enqueue")
   , ("enqueueMultiple", "enqueueMultiple")
   , ("enqueueLater", "enqueueLater")
   , ("dequeue", "dequeue")
   , ("updateCmdMap", "updateCmdMap")
   , ("cmdStatusMap", "cmdStatusMap")
   , ("dequeueFromApi", "dequeueFromApi")

     -- Juno.Consensus.Commit
   , ("doCommit", "doCommit")

     -- Juno.Runtime.Sender
   , ("sendStar", "send*")

     -- Juno.Runtime.MessageReceiver
   , ("messageReceiver", "messageReceiver")

     -- Juno.Consensus.Handle
   , ("handleEvents", "handleEvents")
   , ("handleRPC", "handleRPC")
   , ("issueBatch", "issueBatch")
   -- Juno.Consensus.Handle.AppendEntriesResponse ...
   , ("handleAlotOfAers", "handleAlotOfAers")
   , ("electionTimeoutH", "electionTimeoutH")
   , ("heartbeatTimeoutH", "heartbeatTimeoutH")
   , ("appendEntriesH", "appendEntriesH")
   , ("appendEntriesResponseH", "handle")
   , ("requestVoteH", "requestVoteH")
   , ("requestVoteResponseH", "requestVoteResponseH")
   , ("commandH", "commandH")
   , ("revolutionH", "revolutionH")
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
        applyLogEntry; sendMessage; getMessageRS; getNewCommandsRS; getNewEvidenceRS;
        getRvAndRVRsRS; publishMetric; enqueueRS; enqueueMultiple; enqueueLater; dequeue; updateCmdMap;
        cmdStatusMap; dequeueFromApi;

    cluster (Str "Commit.hsBox") $ do
        graphAttrs [Label (StrLabel "Commit.hs")]
        doCommit

    cluster (Str "Sender.hsBox") $ do
        graphAttrs [Label (StrLabel "Sender.hs")]
        sendStar

    cluster (Str "MessageReceiver.hsBox") $ do
        graphAttrs [Label (StrLabel "MessageReceiver.hs")]
        messageReceiver

    cluster (Str "Handle.hsBox") $ do
        graphAttrs [Label (StrLabel "Handle.hs")]
        handleEvents; handleRPC; issueBatch

    cluster (Str "H.AppendEntriesResponse.hsBox") $ do
        graphAttrs [Label (StrLabel "H.AppendEntriesResponse.hs")]
        handleAlotOfAers; appendEntriesResponseH

    graphAttrs [RankDir FromLeft]
    applyFn;
    inboxWR; outboxWR; rvAndRvrWR; cmdInboxWR; aerInboxWR;
    zmqSocketPull; zmqSocketPush;
    toFromCommands;
    eventWR;
    runCommand; applyFn;
    commandMVarMap;
    runApiServer; apiEnv;
    pubMetric; updateCmdMapFn;
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
    "inboxWR" --> "getMessageRS"
    "cmdInboxWR" --> "getNewCommandsRS"
    "aerInboxWR" --> "getNewEvidenceRS"
    "rvAndRvrWR" --> "getRvAndRVRsRS"
    "publishMetric" --> "pubMetric"
    "enqueueRS" --> "eventWR"
    "enqueueMultiple" --> "eventWR"
    "enqueueLater" --> "eventWR"
    "eventWR" --> "dequeue"
    "updateCmdMapFn" --> "updateCmdMap"
    "commandMVarMap" --> "cmdStatusMap"
    "toFromCommands" --> "dequeueFromApi"

    -- Juno.Consensus.Commit
    "doCommit" --> "applyLogEntry"

    -- Juno.Runtime.Sender
    "sendStar" --> "sendMessage"

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
    edge "issueBatch" "sendStar" [textLabel "sendAllAppendEntries/Response"]
    -- Juno.Consensus.Handle.AppendEntriesResponse
    "handleEvents" --> "handleAlotOfAers"
    -- Juno.Consensus.Handle.ElectionTimeout
    "handleEvents" --> "electionTimeoutH"
    -- Juno.Consensus.Handle.HeartbeatTimeout
    "handleEvents" --> "heartbeatTimeoutH"
    -- NEXT: handleEvents / * fanout
    -- NEXT: handleRPC fanout
    "handleRPC" --> "appendEntriesH"
    "handleRPC" --> "appendEntriesResponseH"
    "handleRPC" --> "requestVoteH"
    "handleRPC" --> "requestVoteResponseH"
    "handleRPC" --> "commandH"
    "handleRPC" --> "revolutionH"

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoServer", junoServer)
            ]
