{-
Created       : 2014 Jul 24 (Thu) 09:37:09 by Harold Carr.
Last Modified : 2018 Jul 24 (Tue) 16:56:54 by Harold Carr.
-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Data.GraphViz                     (GraphvizCommand (Fdp),
                                                    textLabel)
import           Data.GraphViz.Attributes.Complete (Attribute (Height, RankDir, Width),
                                                    RankDir (FromLeft))
import           Data.GraphViz.HC.Util             (doDots', uCircle', uDiamond,
                                                    uDoubleCircle,
                                                    uDoubleOctagon, uFixedSize,
                                                    uRectangle, uRectangle',
                                                    uStar, uTriangle)
import           Data.GraphViz.Types.Generalised   as G (DotGraph)
import           Data.GraphViz.Types.Monadic       (GraphID (Str), digraph,
                                                    edge, graphAttrs, (-->))
import           Data.Text.Lazy                    as L (Text)

------------------------------------------------------------------------------

ui       = uStar          $ uFixedSize [Width 1.5]
behavior = uDoubleCircle  $ uFixedSize [Width 1.5]
event    = uDiamond       $ uFixedSize [Width 1.5, Height 1.5]
handler  = uTriangle      $ uFixedSize [Width 1.5]
fun      = uRectangle     []
mfun     = uDoubleOctagon $ uFixedSize [Width 1.5, Height 1.5]

------------------------------------------------------------------------------

doItBtn        = ui          "doItBtn"            "doItBtn ::\nUI Element"
listBox        = ui          "listBox"            "listBox ::\nUI (ListBox a)"
entry          = ui          "entry"              "entry ::\nBehavior String ->\nUI (TextEntry)"
lbSelection    = ui          "lbSelection"        "lbSelection ::\n UI Element"
userSelection  = fun         "userSelection"      "userSelection ::\nListBox a ->\nTidings (Maybe a)"
rumors         = fun         "rumors"             "rumors ::\nTidings -> \nEvent a"
eLBSelection   = event       "eLBSelection"       "eLBSelection ::\nEvent (Mabye DBKey)"

eFillLB        = event       "eFillLB"            "eFillLB ::\n[(Time,a)]"
hFillLB        = handler     "hFillLB"            "hFillLB ::\na -> IO ()"
on             = fun         "on"                 "on ::\n(element -> Event a)\n-> element\n-> (a -> UI void)\n-> UI ()"
click          = fun         "click"              "click ::\nElement\n-> Event ()"
eDoItClk       = event       "eDoItClk"           "eDoItClk ::\na -> UI void"
doRDFQuery     = mfun        "doRDFQuery"         "doRDFQuery ::\nIO ..."

accumB         = behavior    "accumB"             "accumB ::\nMonadIO m =>\na\n-> Event (a -> a)\n-> m (Behavior a)"
emptydb        = uRectangle' "emptydb"            "emptydb ::\nDB a"
dbFill         = fun         "dbFill"             "dbFill ::\na -> DB DI -> DB DI"
bDB            = behavior    "bDB"                "bDB ::\nBehavior\n(DB DI)"

nothing        = uRectangle' "nothing"            "Nothing"
stepper        = fun         "stepper"            "stepper ::\nMonadIO m =>\na\n-> Event a\n-> m (Behavior a)"
bLBSelection   = behavior    "bLBSelection"       "bLBSelection ::\nBehavior\n(Maybe DBKey)"

bLookup        = behavior    "bLookup"            "bLookup ::\nBehavior\n(DBKey -> Maybe DI)"
bDisplayDI     = behavior    "bDisplayDI"         "bDisplayDI ::\nBehavior\n(DBKey -> UI Element)"

bLBItems       = behavior    "bLBItems"           "bLBItems ::\nBehavior\n[DBKey]"

bLBSelectionDI = behavior    "bLBSelectionDI"     "bLBSelectionDI ::\nBehavior\n(Maybe DI)"

keys           = fun         "keys"               "keys ::\nMap k a -> [k]"

crud2 :: G.DotGraph L.Text
crud2 = digraph (Str "crud2") $ do

    graphAttrs [RankDir FromLeft]
    listBox; eFillLB; hFillLB; doItBtn; lbSelection; eLBSelection; userSelection; rumors;
    on; click; eDoItClk; doRDFQuery;
    accumB; emptydb; dbFill; bDB;
    nothing; stepper; bLBSelection; bLookup; bDisplayDI; bLBItems; bLBSelectionDI; keys;

    edge "bLBItems"      "listBox" [textLabel "1. Behavior [a] -- list of items"]
    edge "bLBSelection"  "listBox" [textLabel "2. Behavior (Maybe a) -- selected item"]
    edge "bDisplayDI"    "listBox" [textLabel "3. Behavior (a -> UI Element) -- display for an item"]

    "bLBSelectionDI" --> "lbSelection"

    "listBox"        --> "userSelection"
    "userSelection"  --> "rumors"
    "rumors"         --> "eLBSelection"
    "eLBSelection"   --> "stepper"
    "nothing"        --> "stepper"
    "stepper"        --> "bLBSelection"

    edge "click"         "on" [textLabel "1."]
    edge "doItBtn"       "on" [textLabel "2."]
    edge "eDoItClk"      "on" [textLabel "3."]
    "eDoItClk"       --> "doRDFQuery"
    "doRDFQuery"     --> "hFillLB"
    "hFillLB"        --> "eFillLB"
    "emptydb"        --> "accumB"
    "eFillLB"        --> "dbFill"
    "dbFill"         --> "accumB"
    "accumB"         --> "bDB"

    "bDB"            --> "bLookup"
    "bLookup"        --> "bDisplayDI"

    "bDB"            --> "keys"
    "keys"           --> "bLBItems"

    "bLBSelection"   --> "bLookup"
    "bLookup"        --> "bLBSelectionDI"

------------------------------------------------------------------------------

bangBang            = uRectangle'    "bangBang"                     "!!"
bFillLB             = behavior       "bFillLB"                      "bFillLB\nm (Behavior a)"
currentValue        = uCircle'       "currentValue"                 "currentValue\nm a"
selectionChange     = uCircle'       "selectionChange"              "selectionChange\nEvent (Maybe Int)"
valuesSupply        = uCircle'       "valuesSupply"                 "valuesSupply\nIO [String]"

taldykin :: G.DotGraph L.Text
taldykin = digraph (Str "taldykin") $ do

    graphAttrs [RankDir FromLeft]
    selectionChange; eFillLB; hFillLB; bFillLB; listBox; valuesSupply; currentValue; bangBang;

    "hFillLB"             --> "eFillLB"
    "eFillLB"             --> "bFillLB"
    "bFillLB"             --> "listBox"
    "bFillLB"             --> "currentValue"
    edge "selectionChange"    "currentValue"         [textLabel "causes"]
    "selectionChange"     --> "bangBang"
    "currentValue"        --> "bangBang"
    "bangBang"            --> "valuesSupply"
    "valuesSupply"        --> "hFillLB"
    "listBox"             --> "selectionChange"

------------------------------------------------------------------------------

acceptLoop6789      = fun         "acceptLoop6789"    "acceptLoop\n6789"
acceptLoop9876      = fun         "acceptLoop9876"    "acceptLoop\n9876"
hAccept             = handler     "hAccept"           "hAccept :: Handler String"
eAccept             = event       "eAccept"           "eAccept ::\nEvent String"
bAccept             = behavior    "bAccept"           "bAccept ::\nm (Behavior String)"
newEvent            = fun         "newEvent"          "newEvent ::\nIO (Event a, Handler a)"

threepennyExternalNewEventDemoCreate :: G.DotGraph L.Text
threepennyExternalNewEventDemoCreate = digraph (Str "threepennyExternalNewEventDemoCreate") $ do

    newEvent; eAccept; hAccept;

    edge "newEvent" "eAccept" [textLabel "creates"]
    edge "newEvent" "hAccept" [textLabel "creates"]

threepennyExternalNewEventDemo :: G.DotGraph L.Text
threepennyExternalNewEventDemo = digraph (Str "threepennyExternalNewEventDemo") $ do

    acceptLoop6789; acceptLoop9876; hAccept; eAccept; stepper; bAccept; entry;

    "acceptLoop6789"     --> "acceptLoop6789"
    "acceptLoop9876"     --> "acceptLoop9876"
    "acceptLoop6789"     --> "hAccept"
    "acceptLoop9876"     --> "hAccept"
    edge "hAccept"            "eAccept" [textLabel "    magic"]
    "eAccept"             --> "stepper"
    "stepper"             --> "bAccept"
    "bAccept"             --> "entry"

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots' "/tmp" Fdp -- TwoPi -- Fdp
            [ ("taldykin"                             , taldykin)
            , ("threepennyExternalNewEventDemoCreate" , threepennyExternalNewEventDemoCreate)
            , ("threepennyExternalNewEventDemo"       , threepennyExternalNewEventDemo)
            , ("crud2"                                , crud2)
            ]

-- End of file.
