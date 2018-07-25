{-
Created       : 2014 Jul 24 (Thu) 09:37:09 by Harold Carr.
Last Modified : 2018 Jul 24 (Tue) 16:49:32 by Harold Carr.
-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Data.GraphViz                     (GraphID (Str),
                                                    GraphvizCommand (Fdp, Fdp))
import           Data.GraphViz.Attributes.Complete (Attribute (RankDir),
                                                    RankDir (FromLeft))
import           Data.GraphViz.HC.Util             (doDots', uRectangle)
import           Data.GraphViz.Types.Generalised   as G (DotGraph)
import           Data.GraphViz.Types.Monadic       (digraph, graphAttrs, (-->))
import           Data.Text.Lazy                    as L (Text)

------------------------------------------------------------------------------

f = uRectangle     []

------------------------------------------------------------------------------

store                = f "store"      "store"

smrFindSM            = f "smrFindSM"           "smrFindSM/*"
qsFindSMByFacets     = f "qsFindSMByFacets"    "qsFindSMByFacets"

smrFindSMId          = f "smrFindSMId"         "smrFindSM/Id"
qsGetMetadataId      = f "qsGetMetadataId"     "qsGetMetadata/Id"

smrFindSMFacets      = f "smrFindSMFacets"     "smrFindSMFacets/*"
qsGetMetadataFacets  = f "qsGetMetadataFacets" "qsGetMetadataFacets/*"

smrCreateSM          = f "smrCreateSM"         "smrCreateSM"
qsAddMetadataId      = f "qsAddMetadataId"     "qsAddMetadata/Id"

smrGetSCategories    = f "smrGetSCategories"   "smrGetSCategories/..."
qsGetSCategories     = f "qsGetSCategories"    "qsGetSCategories"

queryableStore :: G.DotGraph L.Text
queryableStore = digraph (Str "queryableStore") $ do

    graphAttrs [RankDir FromLeft]

    smrFindSM; qsFindSMByFacets;
    smrFindSMId; qsGetMetadataId;
    smrFindSMFacets; qsGetMetadataFacets;
    smrCreateSM; qsAddMetadataId;
    smrGetSCategories; qsGetSCategories;
    store;

    "smrFindSM"         --> "qsFindSMByFacets"
    "qsFindSMByFacets"  --> "store"

    "smrFindSMId"       --> "qsGetMetadataId"
    "qsGetMetadataId"   --> "store"

    "smrFindSMFacets"   --> "qsGetMetadataFacets"
    "qsGetMetadataFacets" --> "store"

    "smrCreateSM"       --> "qsAddMetadataId"
    "qsAddMetadataId"   --> "store"

    "smrGetSCategories" --> "qsGetSCategories"
    "qsGetSCategories"  --> "store"

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots' "/tmp" Fdp -- TwoPi -- Fdp
            [ ("queryableStore" , queryableStore)
            ]

-- End of file.
