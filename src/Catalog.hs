{-
Created       : 2014 Jul 24 (Thu) 09:37:09 by Harold Carr.
Last Modified : 2015 Nov 11 (Wed) 18:18:01 by Harold Carr.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Monad                          (forM_)
import           Data.GraphViz
import           Data.GraphViz.Attributes.Colors.Brewer
import           Data.GraphViz.Attributes.Complete
-- import           Data.GraphViz.HC.Util
import           Data.GraphViz.Types.Generalised        as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                         as L
import           Data.Word                              (Word8 (..))
import           System.FilePath

------------------------------------------------------------------------------

uBaseShape             :: [Attribute] -> n -> Text -> Dot n
uBaseShape      as n l = node n $ [textLabel l, style filled] ++ as

pastel28 :: Word8 -> Attribute
pastel28 n = Color (toColorList [toColor (BC (BScheme Pastel2 8) n)])

uRectangle             :: [Attribute] -> n -> Text -> Dot n
uRectangle      as     = uBaseShape $ [shape     BoxShape,   pastel28 5] ++ as

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

doDots :: PrintDotRepr dg n => [(FilePath, dg n)] -> IO ()
doDots cases = doDots' Dot cases

doDots' :: PrintDotRepr dg n => GraphvizCommand -> [(FilePath, dg n)] -> IO ()
doDots' command cases = forM_ cases (createImage command)

createImage :: PrintDotRepr dg n => GraphvizCommand -> (FilePath, dg n) -> IO FilePath
createImage command (n, g) = createImageInDir command "/tmp" n Png g

createImageInDir :: PrintDotRepr dg n => GraphvizCommand -> FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir c d n o g = Data.GraphViz.addExtension (runGraphvizCommand c g) o (combine d n)

main :: IO ()
main =
    doDots' Fdp -- TwoPi -- Fdp
            [ ("queryableStore" , queryableStore)
            ]

-- End of file.
