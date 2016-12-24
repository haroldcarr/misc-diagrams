{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module JunoCommon where

import           Data.GraphViz                          (GraphvizCommand (TwoPi),
                                                         filled, shape, style,
                                                         textLabel)
import           Data.GraphViz.Attributes.Colors.Brewer (BrewerColor (BC),
                                                         BrewerName (Pastel2),
                                                         BrewerScheme (BScheme))
import           Data.GraphViz.Attributes.Complete      (Attribute (Color, FixedSize, Height, Label, RankDir, Width),
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

function      :: n -> L.Text -> Dot n
function       = uRectangle []

dataStructure :: n -> L.Text -> Dot n
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
   [ -- Client
     ("client","client")

     -- Apps.Juno.Server main
   , ("applyFn","applyFn")

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
   , ("updateCommitIndex", "updateCommitIndex")
   , ("checkCommitProof", "checkCommitProof")

     -- Juno.Runtime.Sender
   , ("sendDummyCollector", "*")
   , ("sendRPC", "sendRPC")
   , ("sendResults", "sendResults")
   , ("sendAppendEntries", "send(All)\nAppendEntries")
   , ("sendAppendEntriesF", "send(All)\nAppendEntries")
   , ("sendAppendEntriesResponse", "send(All)\nAppendEntriesResponse")
   , ("sendAppendEntriesResponseF", "send(All)\nAppendEntriesResponse")

     -- Juno.Runtime.MessageReceiver
   , ("messageReceiver", "messageReceiver")

     -- Juno.Consensus.Handle
   , ("handleEvents", "handleEvents")
   , ("handleEventsF", "handleEvents")
   , ("handleRPC", "handleRPC")
   , ("issueBatch", "issueBatch")
   , ("issueBatchF", "issueBatch")
     -- Juno.Consensus.Handle.AppendEntries
   , ("handleAppendEntries", "handleAppendEntries")
   , ("appendLogEntries", "appendLogEntries")
   , ("addLogEntriesAt", "addLogEntriesAt")
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

     --     Juno.Consensus.Handle.Command
   , ("handleSingleCommand", "handleSingleCommand")
   , ("commandHandle", "commandHandle")

     -- Juno.Runtime.Timer
   , ("electionTimer", "electionTimer")
   , ("heartbeatTimer", "heartbeatTimer")

     -- ...
   , ("makeCommandResponse", "makeCommandResponse")
   , ("applyCommand", "applyCommand")
   , ("apply", "apply")
   , ("applyLogEntries", "applyLogEntries")
   , ("appendLogEntry", "appendLogEntry")
   , ("handleCommand", "handleCommand")
   ]
