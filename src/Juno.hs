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
     ("junoEnv", "JunoEnv")
   , ("toCommands","toCommands")
   , ("fromCommands","fromCommands")
   , ("commandMVarMap","CommandMVarMap")
   , ("runCommand", "runCommand")
   , ("applyFn","applyFn")
     -- Juno.Spec.Simple runJuno
   , ("inboxWrite","inboxWrite")
   , ("inboxRead","inboxRead")
   , ("cmdInboxWrite", "cmdInboxWrite")
   , ("cmdInboxRead", "cmdInboxRead")
   , ("aerInboxWrite", "aerInboxWrite")
   , ("aerInboxRead", "aerInboxRead")
   , ("rvAndRvrWrite","rvAndRvrWrite")
   , ("rvAndRvrRead", "rvAndRvrRead")
   , ("outboxWrite","outboxWrite")
   , ("outboxRead","outboxRead")
   , ("eventWrite","eventWrite")
   , ("eventRead","eventRead")
   , ("pubMetric", "pubMetric")
     -- Juno.Messaging.ZMQ runMsgServer
   , ("zmqSocketPull","zmqSocketPull")
   , ("zmqSocketPush","zmqSocketPush")
     -- Juno.Runtime.Api.ApiServer
   , ("runApiServer","runApiServer")
   , ("apiEnv", "ApiEnv")
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
   , ("sendMessage", "sendMessage")
   , ("sendMessages", "sendMessages")
   , ("getMessage", "getMessage")
   , ("getMessagesRS", "getMessages")
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
   ]

junoServer :: G.DotGraph L.Text
junoServer = digraph (Str "junoServer") $ do
    cluster (Str "ReceiverEnvBox") $ do
        graphAttrs [Label (StrLabel "ReceiverEnv")]
        getMessages; getNewCommands; getNewEvidence; getRvAndRVRs; enqueue

    cluster (Str "RaftSpecBox") $ do
        graphAttrs [Label (StrLabel "RaftSpec")]
        applyLogEntry; sendMessage; sendMessages; getMessage; getMessagesRS; getNewCommandsRS; getNewEvidenceRS;
        getRvAndRVRsRS; publishMetric; enqueueRS; enqueueMultiple; enqueueLater; dequeue; updateCmdMap;
        cmdStatusMap; dequeueFromApi;

    graphAttrs [RankDir FromLeft]
    junoEnv; runCommand; applyFn
    inboxRead; inboxWrite;
    outboxRead; outboxWrite;
    rvAndRvrRead; rvAndRvrWrite;
    cmdInboxRead; cmdInboxWrite;
    aerInboxRead; aerInboxWrite;
    inboxRead; outboxWrite;
    zmqSocketPull; zmqSocketPush;
    fromCommands; toCommands;
    eventRead; eventWrite;
    runCommand; applyFn;
    commandMVarMap;
    runApiServer; apiEnv;
    pubMetric; updateCmdMapFn;

    -- Apps.Juno.Server main
    "toCommands"     -->    "fromCommands"
    "junoEnv"        -->    "runCommand"
    "runCommand"     -->    "applyFn"
    --   Juno.Spec.Simple runJuno

    "inboxWrite" --> "inboxRead"
    "cmdInboxWrite" --> "cmdInboxRead"
    "aerInboxWrite" --> "aerInboxRead"
    "rvAndRvrWrite" --> "rvAndRvrRead"
    "outboxWrite" --> "outboxRead"
    "eventWrite" --> "eventRead"

    -- raftSpec
    "applyFn" --> "applyLogEntry"
    "sendMessage" --> "outboxWrite"
    "sendMessages" --> "outboxWrite"
    "inboxRead" --> "getMessage"
    "inboxRead" --> "getMessagesRS"
    "cmdInboxRead" --> "getNewCommandsRS"
    "aerInboxRead" --> "getNewEvidenceRS"
    "rvAndRvrRead" --> "getRvAndRVRsRS"
    "pubMetric" --> "publishMetric"
    "enqueueRS" --> "eventWrite"
    "enqueueMultiple" --> "eventWrite"
    "enqueueLater" --> "eventWrite"
    "eventRead" --> "dequeue"
    "updateCmdMapFn" --> "updateCmdMap"
    "commandMVarMap" --> "cmdStatusMap"
    "fromCommands" --> "dequeueFromApi"

    -- ReceiverEnv : MessageReceiver / Simple
    "inboxRead" --> "getMessages"
    "cmdInboxRead" --> "getNewCommands"
    "aerInboxRead" --> "getNewEvidence"
    "rvAndRvrRead" --> "getRvAndRVRs"
    "enqueue" --> "eventWrite"

    --     Juno.Runtime.Api.ApiServer
    "commandMVarMap" --> "runApiServer"
    "runApiServer"   --> "toCommands"
    "apiEnv" --> "runApiServer"

    --     Juno.Messaging.ZMQ runMsgServer
    edge "zmqSocketPull"    "rvAndRvrWrite" [textLabel "RV | RVR"]
    edge "zmqSocketPull"    "cmdInboxWrite" [textLabel "CMD | CMDB"]
    edge "zmqSocketPull"    "aerInboxWrite" [textLabel "AER"]
    edge "zmqSocketPull"    "inboxWrite"    [textLabel "otherwise"]

    edge "outboxRead"       "zmqSocketPush" [textLabel "send rolodex"]

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoServer", junoServer)
            ]
