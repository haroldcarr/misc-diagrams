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

junoAER :: G.DotGraph L.Text
junoAER = digraph (Str "junoAER") $ do
    cluster (Str "RaftStateBox") $ do
        graphAttrs [Label (StrLabel "RaftState")]
        commitProof
    cluster (Str "CommitHSBox") $ do
        graphAttrs [Label (StrLabel "Commit.hs")]
        doCommit
    cluster (Str "AERRaftStateBox1") $ do
        graphAttrs [Label (StrLabel "AER RaftState")]
        aerhandle; aeramIActiveNow; aerIsActive; aerActiveInIndex; aerActiveInIndexP1; aerignore;
        insideCIP1; handleReputableAER; fstActAsmntAfterCI; lbAAEqAERIndexP1; evalRecoveryOnInactiveAER;
    cluster (Str "AERRaftStateBox2") $ do
        graphAttrs [Label (StrLabel "AER RaftState")]
        aermergeCommitProof;
    cluster (Str "PureBox") $ do
        graphAttrs [Label (StrLabel "AER pure")]
        handleAEResponse; updateCommitProofMap; aerisLeader; caseConvincedSuccessTerm; aerCommitProof;
        aerUCDeleteRET; aerResetElectionTimer; aerUPLNext; aerUCInsertRET;

    graphAttrs [RankDir FromLeft]
    -- PURE
    "handleAEResponse" --> "updateCommitProofMap"
    "updateCommitProofMap" --> "aerisLeader"
    edge "aerisLeader" "aerCommitProof" [textLabel "no"]
    edge "aerisLeader" "caseConvincedSuccessTerm" [textLabel "yes"]
    edge "caseConvincedSuccessTerm" "aerCommitProof" [textLabel "isInfo?"]
    edge "caseConvincedSuccessTerm" "aerUCDeleteRET" [textLabel "(NotCnv,_,Old | CurReqTerm)"]
    edge "caseConvincedSuccessTerm" "aerResetElectionTimer" [textLabel "(NotCnv,_,NewReqTerm)"]
    edge "caseConvincedSuccessTerm" "aerUPLNext" [textLabel "(Cnv,Fail,CurReqTerm)"]
    edge "caseConvincedSuccessTerm" "aerUCInsertRET" [textLabel "(Cnv,Succ,CurReqTerm)"]
    edge "caseConvincedSuccessTerm" "aerResetElectionTimer" [textLabel "(Cnv,Fail,_)"]
    edge "caseConvincedSuccessTerm" "aerResetElectionTimer" [textLabel "(Cnv,Succ,OldReqTerm)"]
    edge "caseConvincedSuccessTerm" "aerResetElectionTimer" [textLabel "(_,_,NewReqTerm)"]
    -------------------------
    -- Raft1
    "aerhandle" --> "aeramIActiveNow"
    "aeramIActiveNow" --> "aerIsActive"
    edge "aerIsActive" "aerActiveInIndex" [textLabel "yes"]
    edge "aerIsActive" "aerActiveInIndexP1" [textLabel "no"]
    edge "aerActiveInIndex" "aerignore" [textLabel "no"]
    edge "aerActiveInIndexP1" "aerignore" [textLabel "no"]
    "aerActiveInIndex" --> "insideCIP1"
    edge "insideCIP1" "aerignore" [textLabel "no"]
    edge "insideCIP1" "handleReputableAER" [textLabel "yes"]
    "aerActiveInIndexP1" --> "fstActAsmntAfterCI"
    edge "fstActAsmntAfterCI" "aerignore" [textLabel "no"]
    edge "fstActAsmntAfterCI" "lbAAEqAERIndexP1" [textLabel "yes"]
    edge "lbAAEqAERIndexP1" "handleReputableAER" [textLabel "1"]
    edge "lbAAEqAERIndexP1" "evalRecoveryOnInactiveAER" [textLabel "2"]
    "handleReputableAER" --> "handleAEResponse"
    -------------------------
    -- Raft2
    "aerCommitProof" --> "aermergeCommitProof"
    "aerUCDeleteRET" --> "aermergeCommitProof"
    "aerResetElectionTimer" --> "aermergeCommitProof"
    "aerUPLNext" --> "aermergeCommitProof"
    "aerUCInsertRET" --> "aermergeCommitProof"
    "aermergeCommitProof" --> "commitProof"
    "aermergeCommitProof" --> "doCommit"

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoAER"   , junoAER)
            ]
