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

junoCommit :: G.DotGraph L.Text
junoCommit = digraph (Str "junoCommit") $ do
    cluster (Str "CommitHSBox") $ do
        graphAttrs [Label (StrLabel "Commit.hs")]
        doCommit

    graphAttrs [RankDir FromLeft]
    -- PURE
    -------------------------
    -- Raft
    "doCommit" --> "doCommit"

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots "/tmp"
            [ ("junoCommit"   , junoCommit)
            ]
