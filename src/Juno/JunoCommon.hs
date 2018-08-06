{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Juno.JunoCommon where

import           Data.GraphViz.HC.DiagramsTH (mk)
import           Data.GraphViz.HC.Util       (uCircle', uDiamond', uRectangle)
import           Data.GraphViz.Types.Monadic (Dot)
import qualified Data.Text.Lazy              as L (Text)
default (L.Text)

function      :: n -> L.Text -> Dot n
function       = uRectangle []

dataStructure :: n -> L.Text -> Dot n
dataStructure  = uCircle'

predicate     :: n -> L.Text -> Dot n
predicate      = uDiamond'

------------------------------------------------------------------------------
-- SERVER

mk "dataStructure"
   [ -- Apps.Juno.Server main
     ("toFromCommands","toFrom\nCommands")
   , ("commandMVarMap","Command\nMVarMap")

     -- App.Juno.Command
   , ("junoEnv", "JunoEnv")
   , ("appState", "App\nState")
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
   , ("commitIndex" , "commit\nIndex")
   , ("commitProof" , "commit\nProof")
   , ("commitProofF" , "commit\nProof")
   , ("convinced", "convinced")
   , ("currentLeader" , "current\nLeader")
   , ("lastCommitTime", "last\nCommit\nTime")
   , ("lastCompleted", "last\nCompleted")
   , ("logEntries" , "log\nEntries")
   , ("logEntriesF" , "log\nEntries")
   , ("lNextIndex", "lNextIndex")
   , ("membershipState", "membership\nState")
   , ("quorumSize" , "quorum\nSize")
   , ("recentAndTentativeStates", "rctTent\nStates")
   , ("replayMap", "replay\nMap")
   , ("replayMapF", "replay\nMap")
   , ("term", "term")
   , ("timeSinceLastAER", "timeSince\nLastAER")
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
   , ("runOrCommitCommand", "runCommand\ncommitCommand")

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
   , ("sendRPCs", "send\nRPCs")
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

   , ("appendEntriesH", "appendEntriesH")
   , ("commandH", "commandH")
   , ("commandResponseH", "command\nResponseH")
   , ("cqH", "cqH")
   , ("cqrH", "cqrH")
   , ("electionTimeoutH", "electionTimeoutH")
   , ("heartbeatTimeoutH", "heartbeatTimeoutH")
   , ("msdH", "msdH")
   , ("requestVoteH", "requestVoteH")
   , ("requestVoteResponseH", "requestVoteResponseH")
   , ("revolutionH", "revolutionH")

     -- Juno.Consensus.Handle.Command
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

------------------------------------------------------------------------------
-- AppendEntries

mk "dataStructure"
   [ ("ignore", "Ignore")
   , ("sendUnconvincedResponse", "Send\nUnconvinced\nResponse")
   , ("validLeaderAndTerm", "ValidLeader\nAndTerm")
   , ("sendFailureResponse", "SendFailure\nResponse")
   ]

mk "function"
   [ -- pure
     ("aehandleAppendEntries", "handle\nAppend\nEntries*")
   , ("myActiveAssignments", "myActive\nAsmnts")
   , ("checkForNewLeader", "checkFor\nNewLeader")
   , ("dropIdenticalLEs", "drop\nIdentical\nLEs")
   , ("aeappendLogEntries", "append\nLogEntries")
     -- Raft
   , ("applyNewLeader", "apply\nNewLeader")
   , ("aesendAppendEntriesResponse1", "sendAppend\nEntries\nResponse")
   , ("aesendAppendEntriesResponse2", "sendAppend\nEntries\nResponse")
   , ("aesendAppendEntriesResponse3", "sendAppend\nEntries\nResponse")
   , ("aesendAppendEntriesResponse4", "sendAppend\nEntries\nResponse")
   , ("aesendAppendEntriesResponse5", "sendAppend\nEntries\nResponse")
   , ("aesendAllAppendEntriesResponse", "sendAllAppend\nEntries\nResponse")
   , ("resetElectionTimer", "reset\nElection\nTimer")
   , ("setLazyVoteToNothing", "setLazyVote\n.= Nothing")
   , ("validateProposedLogEntries", "validateProposed\nLogEntries")
   , ("applyCommandAndCompareLogEntries", "applyCommandAnd\nCompareLogEntries")
   , ("recoverPage", "recover\nPage")
   , ("whenRemovedEntriesJunoBFTFlaw", "whenRemoved\nEntries\nJuno BFT Flaw")
   , ("addLogEntriesAfter", "addLog\nEntriesAfter")
   , ("aeerror", "error")
   , ("removeUnfinishedReplaysAddNew", "remove\nUnfinished\nReplays\nAdd New")
   , ("updateRecentAndTentativeStates", "update\nRecentAnd\nTentativeStates")
   , ("aeupdateCommitProofMap", "update\nCommit\nProofMap")
   ]

mk "predicate"
   [ --  pure
     ("isActiveP", "active?")
   , ("prevLI_GE_CI_or_nullLEs", "prevLI >= CI\n|| null LEs")
   , ("prevLI_plus_lenLEs_LE_CI", "prevLI + lenLEs\n<= CI")
   , ("prevLEMatches", "prev LE\nmatchs")
   , ("caseCurrentLeader", "current\nLeader")
     -- Raft
   , ("casePreState", "case\nPreState")
   , ("validResponse", "valid\nResponse")
   ]

------------------------------------------------------------------------------
-- AppendEntriesResponse

mk "dataStructure"
   [ ("aerCommitProof", "ComPrf")
   , ("aerUCDeleteRET", "ComPrf,\nUpCnvnc/Del,\nResetET")
   , ("aerResetElectionTimer", "ComPrf,\nResetET")
   , ("aerUPLNext", "ComPrf,\nUpLNxt,\nResetET")
   , ("aerUCInsertRET", "ComPrf,\nUpCnvnc/Ins,\nResetET,\nUpLNxt")
   ]

mk "function"
   [ -- pure
     ("handleAEResponse", "handleAE\nResponse")
     -- Raft
   , ("aerhandle", "handle")
   , ("aeramIActiveNow", "amIActiveNow")
   , ("aerignore", "ignore")
   , ("handleReputableAER", "handle\nReputable\nAER")
   , ("evalRecoveryOnInactiveAER", "evalRecovery\nOnInactiveAER")
   , ("aermergeCommitProof", "merge\nCommitProof")
   ]

mk "predicate"
   [ --  pure
     ("aerisLeader", "leader?")
   , ("caseConvincedSuccessTerm", "case\n(convinced,\nsuccess,\nterm)")
     -- Raft
   , ("aerIsActive", "active?")
   , ("aerActiveInIndex", "active in\naerIndex?")
   , ("aerActiveInIndexP1", "active in\naerIndex + 1?")
   , ("insideCIP1", "inside\n(ci + 1)")
   , ("fstActAsmntAfterCI", "1st\nactive\nAsmnt\nAfter\nCI")
   , ("lbAAEqAERIndexP1", "lowerBound\nactive interval\n==\naerIndex + 1)")
   ]

------------------------------------------------------------------------------
-- Commit

mk "dataStructure"
   [ -- pure
     ("ccpRight", "Right\nIX.head.\ndrop (qsize-1)\nFE")
     -- Raft
   ]

mk "function"
   [ -- pure
     ("cmtcheckCommitProof", "checkCommitProof")
   , ("cmtfinalEvidence", "finalEvidence")
   , ("filterEQCT", "termCheck: filter\n== crntTerm")
   , ("filterGCIandLEQMaLI", "prepare: filter\n> cur Ix\n&& >= maxLogIx")
   , ("hashChecks", "hashChecks: filter\nhash AER ==\nhash (lkup (IX AER) log)")
   , ("sortByDownIx", "sortBy\nDown Ix")
     -- Raft
   , ("cmtdoCommit", "doCommit")
   , ("cmtupdateCommitIndex", "update\nCommitIndex")
   , ("qciGTci", "qci > ci")
   , ("setcommitIndexToQCI", "commitIndex\n.= qci")
   , ("filterQCILTaerIndex", "discard used evidenc\nfilter\n((qci <) . _aerIndex)")
   , ("logCommitChange", "log\nCommit\nChange")
   , ("getTime", "getTime")
   , ("completeCommittedLogEntries", "complete\nCommitted\nLogEntries")
   , ("getEntriesInLeftHalfOpenInterval", "getEntriesInLeft\nHalfOpenInterval")
   , ("getAndSendResults", "getAndSendResults")
   , ("completeCommand", "complete\nCommand")
   , ("commitLogEntry","commit\nLogEntry")
   ]

mk "predicate"
   [ --  pure
     -- Raft
   ]


