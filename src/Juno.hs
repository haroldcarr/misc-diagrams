{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Data.GraphViz                          (filled, shape, style,
                                                         textLabel, GraphvizCommand(TwoPi))
import           Data.GraphViz.Attributes.Colors.Brewer (BrewerColor (BC),
                                                         BrewerName (Pastel2),
                                                         BrewerScheme (BScheme))
import           Data.GraphViz.Attributes.Complete      (Attribute (Color, FixedSize, Height, RankDir, Width),
                                                         Color (RGB), ColorList (..),
                                                         NodeSize (SetNodeSize),
                                                         Number (Int),
                                                         RankDir (FromLeft), Shape (Circle, BoxShape, DiamondShape, DoubleCircle),
                                                         toColor, toColorList)
import           Data.GraphViz.HC.Util                  (doDots)
import qualified Data.GraphViz.Types.Generalised        as G (DotGraph)
import           Data.GraphViz.Types.Monadic            (Dot,
                                                         GraphID (Str, Num),
                                                         cluster, digraph, edge,
                                                         graphAttrs, node,
                                                         (-->))
import qualified Data.Text                              as T (Text)
import           Data.Text.Lazy                         as L (Text)
import           Data.Word                              (Word8)
import           DiagramsTH                             (mk)
default (T.Text)

------------------------------------------------------------------------------
-- DIAGRAM HELPER FUNCTIONS

-- http://www.colorcombos.com/color-schemes/2025/ColorCombo2025.html
myColorCL :: Word8 -> ColorList
myColorCL n | n == 1 = c (RGB 127 108 138)
            | n == 2 = c (RGB 175 177 112)
            | n == 3 = c (RGB 226 206 179)
            | n == 4 = c (RGB 172 126 100)
  where c rgb = toColorList [rgb]

myColor  :: Word8 -> Attribute
myColor n = Color $ myColorCL n

pastel28                            :: Word8 -> Attribute
pastel28 n                          = Color (toColorList [toColor (BC (BScheme Pastel2 8) n)])

startEndClosedState                 :: L.Text -> L.Text -> Dot L.Text
startEndClosedState n l             = node n [textLabel l, shape DoubleCircle, pastel28 1, style filled, FixedSize SetNodeSize, Width 1]

state                               :: L.Text -> L.Text -> Dot L.Text
state               n l             = node n [textLabel l, shape       Circle, pastel28 2, style filled, FixedSize SetNodeSize, Width 1]

transition                          :: L.Text -> L.Text -> Dot L.Text
transition          n l             = node n [textLabel l, shape     BoxShape, pastel28 5, style filled]

decision                            :: L.Text -> L.Text -> Dot L.Text
decision            n l             = node n [textLabel l, shape DiamondShape, pastel28 6, style filled, FixedSize SetNodeSize, Width 1.5, Height 1.5]

rectangle    :: n -> Text -> Dot n
rectangle n l = node n [textLabel l, shape BoxShape,  Width 1, style filled, myColor 3]

------------------------------------------------------------------------------
-- SERVER

mk [ ("inboxWrite","inboxWrite")
   , ("inboxRead","inboxRead")
   , ("inboxRead'", "inboxRead'")
   , ("cmdInboxWrite", "cmdInboxWrite")
   , ("cmdInboxRead", "cmdInboxRead")
   , ("cmdInboxRead'", "cmdInboxRead'")
   , ("aerInboxWrite", "aerInboxWrite")
   , ("aerInboxRead", "aerInboxRead")
   , ("aerInboxRead'", "aerInboxRead'")
   , ("rvAndRvrWrite","rvAndRvrWrite")
   , ("rvAndRvrRead", "rvAndRvrRead")
   , ("outboxWrite","outboxWrite")
   , ("outboxRead","outboxRead")
   , ("eventWrite","eventWrite")
   , ("eventRead","eventRead")
   , ("pubMetric", "pubMetric")
   , ("zmqSocketPull","zmqSocketPull")
   , ("zmqSocketPush","zmqSocketPush")
   ]


runJuno :: G.DotGraph L.Text
runJuno = digraph (Str "runJuno") $ do
    graphAttrs [RankDir FromLeft]
    zmqSocketPull; rvAndRvrWrite; cmdInboxWrite; aerInboxWrite; inboxWrite;
    outboxRead; zmqSocketPush;

    -- Juno.Spec.Simple runJuno
    --   runMsgServer inboxWrite cmdInboxWrite aerInboxWrite rvAndRvrWrite outboxRead me [] moreLogging
    edge "zmqSocketPull"    "rvAndRvrWrite" [textLabel "RV | RVR"]
    edge "zmqSocketPull"    "cmdInboxWrite" [textLabel "CMD | CMDB"]
    edge "zmqSocketPull"    "aerInboxWrite" [textLabel "AER"]
    edge "zmqSocketPull"    "inboxWrite"    [textLabel "otherwise"]

    edge "outboxRead"       "zmqSocketPush" [textLabel "map send rolodex"]

main :: IO ()
main =
    doDots "/tmp"
            [ ("runJuno", runJuno)
            ]
