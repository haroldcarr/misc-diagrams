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

junoCommand :: G.DotGraph L.Text
junoCommand = digraph (Str "junoCommand") $ do
    cluster (Str "RaftStateBox") $
        graphAttrs [Label (StrLabel "RaftState")]
    cluster (Str "CommandRaftStateBox") $
        graphAttrs [Label (StrLabel "Command RaftState")]
    cluster (Str "PureBox") $
        graphAttrs [Label (StrLabel "Command pure")]

    graphAttrs [RankDir FromLeft]
    -- PURE
    -------------------------
    -- Raft1

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoCommand"   , junoCommand)
            ]
