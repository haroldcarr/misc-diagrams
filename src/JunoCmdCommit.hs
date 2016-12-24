{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           JunoCommon

import           Data.GraphViz                          (GraphvizCommand (TwoPi),
                                                         filled, shape, style,
                                                         textLabel)
import           Data.GraphViz.Attributes.Colors.Brewer (BrewerColor (BC),
                                                         BrewerName (Pastel2),
                                                         BrewerScheme (BScheme))
import           Data.GraphViz.Attributes.Complete      (Attribute (Color, Compound, FixedSize, Height, Label, RankDir, Width),
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

------------------------------------------------------------------------------

junoCmdCommit :: G.DotGraph L.Text
junoCmdCommit = digraph (Str "junoCmdCommit") $ do

    graphAttrs [ {- RankDir FromLeft, -} Compound True]

    client;

    "client" --> "handleEvents"

    cluster (Str "leaderId") $ do
        graphAttrs [Label (StrLabel "leader")]
        handleEvents; handleCommand;
        issueBatch; doCommit; applyLogEntries; applyCommand; apply;
        appendLogEntry; sendResults; sendAppendEntries; sendAppendEntriesResponse;
        updateCommitIndex; checkCommitProof;

    edge "handleEvents"         "handleCommand" [textLabel "1"]
    "handleCommand" -->         "appendLogEntry"

    edge "handleEvents"         "issueBatch" [textLabel "2"]
    edge "issueBatch"           "doCommit" [textLabel "1"]
    edge "doCommit"             "updateCommitIndex" [textLabel "1"]
    "updateCommitIndex" -->     "checkCommitProof"
    edge "doCommit"             "applyLogEntries" [textLabel "2 when quorum"]
    edge "applyLogEntries"      "applyCommand" [textLabel "1"]
    edge "applyCommand"         "apply"  [textLabel "app-specific"]
    edge "applyLogEntries"      "sendResults" [textLabel "2 to clients"]
    "sendResults" -->           "client"

    edge "issueBatch"           "sendAppendEntriesResponse" [textLabel "2"]
    edge "issueBatch"           "sendAppendEntries" [textLabel "3"]
    "sendAppendEntries" -->     "handleEventsF"

    cluster (Str "followerId") $ do
        graphAttrs [Label (StrLabel "follower")]
        handleEventsF; issueBatchF; handleAppendEntries; appendLogEntries; addLogEntriesAt;
        sendAppendEntriesResponseF;

    edge "handleEventsF"        "handleAppendEntries" [textLabel "1"]
    edge "handleAppendEntries"  "appendLogEntries"    [textLabel "1"]
    "appendLogEntries" -->      "addLogEntriesAt"
    edge "handleAppendEntries"  "sendAppendEntriesResponseF" [textLabel "2"]

    edge "handleEventsF"        "issueBatchF"  [textLabel "2"]

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoCmdCommit", junoCmdCommit)
            ]
