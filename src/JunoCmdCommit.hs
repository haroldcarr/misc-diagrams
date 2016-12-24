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

------------------------------------------------------------------------------

junoCmdCommit :: G.DotGraph L.Text
junoCmdCommit = digraph (Str "junoCmdCommit") $ do
    handleEvents; handleRPC; handleSingleCommand; handleCommand;
    issueBatch; doCommit; applyLogEntries; applyCommand; apply; makeCommandResponse;
    appendLogEntry;

    edge "handleEvents" "handleRPC" [textLabel "1"]
    "handleRPC" --> "handleSingleCommand"
    "handleSingleCommand" --> "handleCommand"
    edge "handleCommand" "handleSingleCommand" [textLabel "CommitAndPropagate (LogEntry term cmd B.empty)"]
    "handleCommand" --> "appendLogEntry"
    edge "handleEvents" "issueBatch" [textLabel "2"]
    "issueBatch" --> "doCommit"
    "doCommit" --> "applyLogEntries"
    edge "applyLogEntries" "applyCommand" [textLabel "1"]
    edge "applyCommand" "apply"  [textLabel "apply is app-specific"]
    "applyCommand" --> "makeCommandResponse"

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoCmdCommit", junoCmdCommit)
            ]
