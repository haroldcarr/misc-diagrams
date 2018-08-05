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
    cluster (Str "RaftStateBox") $
        graphAttrs [Label (StrLabel "RaftState")]
    cluster (Str "AERRaftStateBox") $
        graphAttrs [Label (StrLabel "AER RaftState")]
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
    -- Raft

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoAER"   , junoAER)
            ]
