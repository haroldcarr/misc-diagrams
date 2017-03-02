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
     -- Juno.Types.Spec
   , ("logEntries" , "log\nEntries")
   , ("logEntriesF" , "log\nEntries")
   , ("commitProof" , "commit\nProof")
   , ("commitProofF" , "commit\nProof")
   , ("replayMap", "replay\nMap")
   , ("replayMapF", "replay\nMap")
   ]

mk "function"
   [ -- misc
     ("follower","follower")

     -- Client
   , ("client","client")

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
   , ("applyLogEntry", "apply\nLogEntry")
   , ("sendMessage", "sendMessage(s)")
   , ("publishMetric", "publishMetric")
   , ("enqueueMultiple", "enqueueMultiple")
   , ("enqueueLater", "enqueueLater")
   , ("dequeue", "dequeue")
   , ("updateCmdMap", "updateCmdMap")
   , ("dequeueFromApi", "dequeueFromApi")

     -- Juno.Consensus.Commit
   , ("doCommit", "doCommit")
   , ("updateCommitIndex", "update\nCommitIndex")
   , ("checkCommitProof", "check\nCommitProof")

     -- Juno.Runtime.Sender
   , ("sendDummyCollector", "*")
   , ("sendRPC", "send\nRPC")
   , ("sendResults", "send\nResults")
   , ("sendAppendEntries", "send(All)\nAppendEntries")
   , ("sendAppendEntriesF", "send(All)\nAppendEntries")
   , ("sendAppendEntriesResponse", "send(All)Append\nEntriesResponse")
   , ("sendAppendEntriesResponseF", "send(All)Append\nEntriesResponse")

     -- Juno.Runtime.MessageReceiver
   , ("messageReceiver", "messageReceiver")

     -- Juno.Consensus.Handle
   , ("handleEvents", "handle\nEvents")
   , ("handleEventsF", "handle\nEvents")
   , ("handleRPC", "handleRPC")
   , ("issueBatch", "issue\nBatch")
   , ("issueBatchF", "issue\nBatch")
     -- Juno.Consensus.Handle.AppendEntries
   , ("handleAppendEntries", "handle\nAppendEntries")
   , ("appendLogEntries", "append\nLogEntries")
   , ("addLogEntriesAt", "addLog\nEntriesAt")
   , ("updateLogHashesFromIndex", "updateLogHashes\nFromIndex")
   , ("verifyLogHashesFromIndex", "verifyLogHashes\nFromIndex")
   , ("validResponses", "valid\nResponses")
     -- Juno.Consensus.Handle.AppendEntriesResponse ...
   , ("handleAlotOfAers", "handleAlotOfAers")
   , ("appendEntriesResponseH", "AER.handle")
   , ("updateCommitProofMap", "updateCommit\nProofMap")
   , ("updateCommitProofMapF", "updateCommit\nProofMap")

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
   , ("applyCommand", "apply\nCommand")
   , ("apply", "APP-SPECIFIC\nAPPLY")
   , ("applyF", "APP-SPECIFIC\nAPPLY")
   , ("applyLogEntries", "apply\nLogEntries")
   , ("appendLogEntry", "append\nLogEntry")
   , ("handleCommand", "handle\nCommand")
   ]
