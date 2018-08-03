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

junoAE :: G.DotGraph L.Text
junoAE = digraph (Str "junoAE") $ do
    cluster (Str "RaftStateBox") $ do
        graphAttrs [Label (StrLabel "RaftState")]
        logEntries; replayMap; recentAndTentativeStates; commitProof;
    cluster (Str "AERaftStateBox") $ do
        graphAttrs [Label (StrLabel "AE RaftState")]
        applyNewLeader;
        aesendAppendEntriesResponse1; aesendAppendEntriesResponse2; aesendAppendEntriesResponse3;
        aesendAppendEntriesResponse4; aesendAppendEntriesResponse5; aesendAllAppendEntriesResponse;
        resetElectionTimer; setLazyVoteToNothing;
        casePreState; validateProposedLogEntries; applyCommandAndCompareLogEntries;
        validResponse; recoverPage; whenRemovedEntriesJunoBFTFlaw; addLogEntriesAfter;
        aeerror; removeUnfinishedReplaysAddNew; updateRecentAndTentativeStates;
        aeupdateCommitProofMap;
    cluster (Str "PureBox") $ do
        graphAttrs [Label (StrLabel "AE pure")]
        aehandleAppendEntries; myActiveAssignments; checkForNewLeader;
        isActiveP; ignore;
        prevLI_GE_CI_or_nullLEs;
        prevLI_plus_lenLEs_LE_CI; dropIdenticalLEs;
        caseCurrentLeader; sendUnconvincedResponse; prevLEMatches;
        aeappendLogEntries; validLeaderAndTerm;

    graphAttrs [RankDir FromLeft]

    -- PURE
    "aehandleAppendEntries" --> "myActiveAssignments"
    "aehandleAppendEntries" --> "checkForNewLeader"
    "myActiveAssignments" --> "isActiveP"
    "checkForNewLeader" --> "isActiveP"
    edge "isActiveP"    "ignore" [textLabel "no"]
    edge "isActiveP"    "prevLI_GE_CI_or_nullLEs" [textLabel "yes"]
    edge "prevLI_GE_CI_or_nullLEs" "dropIdenticalLEs"  [textLabel "yes;\nall LEs"]
    edge "prevLI_GE_CI_or_nullLEs" "prevLI_plus_lenLEs_LE_CI" [textLabel "no"]
    edge "prevLI_plus_lenLEs_LE_CI" "dropIdenticalLEs" [textLabel "yes\nalready committed;\nempty LEs"]
    edge "prevLI_plus_lenLEs_LE_CI" "dropIdenticalLEs" [textLabel "no;\nuncommited\nLEs"]
    "dropIdenticalLEs" --> "caseCurrentLeader"
    edge "caseCurrentLeader" "sendUnconvincedResponse" [textLabel "Nothing"]
    edge "caseCurrentLeader" "prevLEMatches" [textLabel "not ignore\nsame leader\nand term"]
    edge "prevLEMatches" "validLeaderAndTerm" [textLabel "no;\nSendFailure\nResponse"]
    edge "prevLEMatches" "aeappendLogEntries" [textLabel "yes"]
    edge "aeappendLogEntries" "validLeaderAndTerm" [textLabel "yes;\nCommit"]
    edge "caseCurrentLeader" "sendUnconvincedResponse" [textLabel "not ignore\naeTerm >=\ncurrent\nterm"]
    edge "caseCurrentLeader" "ignore" [textLabel "not ignore\naeTerm <\ncurrent\nterm"]
    edge "caseCurrentLeader" "ignore" [textLabel "else"]
    -------------------------
    -- Raft
    "ignore"  --> "applyNewLeader"
    "sendUnconvincedResponse" --> "applyNewLeader"
    edge "sendUnconvincedResponse" "aesendAppendEntriesResponse1" [textLabel "lid\nFailure\nNotConvinced"]
    "validLeaderAndTerm" --> "applyNewLeader"
    edge "validLeaderAndTerm" "resetElectionTimer" [textLabel "Commit"]
    "resetElectionTimer" --> "setLazyVoteToNothing"
    edge "setLazyVoteToNothing" "aesendAppendEntriesResponse2" [textLabel "SFR:\nlid\nFailure\nConvinced"]
    edge "setLazyVoteToNothing" "casePreState" [textLabel "Commit"]
    edge "casePreState" "aesendAppendEntriesResponse3" [textLabel "Nothing;\nlid\nFailure\nConvinced"]
    "casePreState" --> "validateProposedLogEntries"
    "validateProposedLogEntries" --> "applyCommandAndCompareLogEntries"
    "applyCommandAndCompareLogEntries" --> "validateProposedLogEntries"
    "validateProposedLogEntries" --> "validResponse"
    edge "validResponse" "recoverPage" [textLabel "MissingPage"]
    edge "validResponse" "aesendAppendEntriesResponse4" [textLabel "InvalidEntry:\nlid Failure Convinced"]
    edge "validResponse" "whenRemovedEntriesJunoBFTFlaw" [textLabel "NewStates"]
    "whenRemovedEntriesJunoBFTFlaw" --> "addLogEntriesAfter"
    "addLogEntriesAfter" --> "logEntries"
    edge "addLogEntriesAfter" "aeerror" [textLabel "mylog /= updatedLog'"]
    edge "addLogEntriesAfter" "aeerror" [textLabel "remlog /= removedEntries"]
    edge "addLogEntriesAfter" "aeerror" [textLabel "length newStates /= length appliedLEs"]
    "addLogEntriesAfter" --> "removeUnfinishedReplaysAddNew"
    "removeUnfinishedReplaysAddNew" --> "replayMap"
    "removeUnfinishedReplaysAddNew" --> "updateRecentAndTentativeStates"
    "updateRecentAndTentativeStates" --> "recentAndTentativeStates"
    "updateRecentAndTentativeStates" --> "aeupdateCommitProofMap"
    "aeupdateCommitProofMap" --> "commitProof"
    edge "aeupdateCommitProofMap" "aesendAppendEntriesResponse5" [textLabel "lid Success Convinced"]
    "aesendAppendEntriesResponse5" --> "aesendAllAppendEntriesResponse"

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoAE"   , junoAE)
            ]
