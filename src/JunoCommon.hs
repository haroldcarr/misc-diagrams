{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module JunoCommon where

import           Data.GraphViz.HC.DiagramsTH (mk)
import           Data.GraphViz.HC.Util       (uCircle', uRectangle)
import           Data.GraphViz.Types.Monadic (Dot)
import qualified Data.Text.Lazy              as L (Text)
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
   , ("commitIndex" , "commit\nIndex")
   , ("commitProof" , "commit\nProof")
   , ("commitProofF" , "commit\nProof")
   , ("replayMap", "replay\nMap")
   , ("replayMapF", "replay\nMap")
   , ("membershipState", "membership\nState")
   , ("recentAndTentativeStates", "rctTent\nStates")
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
   , ("handleRPC", "handle\nRPC")
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
   , ("handleAlotOfAers", "AER\nhandleAlotOfAers")
   , ("appendEntriesResponseH", "AER\nhandle")
   , ("updateCommitProofMap", "AER\nupdateCommit\nProofMap")
   , ("updateCommitProofMapF", "AER\nupdateCommit\nProofMap")

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
   , ("commitAndPropagateCollector", "*")

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
