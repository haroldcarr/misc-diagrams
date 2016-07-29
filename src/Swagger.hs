{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.GraphViz                     (GraphID (Str),
                                                    Shape (BoxShape), filled,
                                                    shape, style, textLabel)
import           Data.GraphViz.Attributes.Complete (Attribute (Color, Compound, FixedSize, Label, LHead, RankDir, Width),
                                                    Color (RGB), ColorList (..),
                                                    Label (StrLabel),
                                                    NodeSize (SetNodeSize),
                                                    RankDir (FromLeft),
                                                    toColorList)
import           Data.GraphViz.HC.Util             (colorCombo2025, doDots, uRectangle)
import           Data.GraphViz.HC.DiagramsTH       (mk)
import           Data.GraphViz.Types.Generalised   as G (DotGraph)
import           Data.GraphViz.Types.Monadic       (Dot, cluster, digraph, edge,
                                                    graphAttrs, node, (-->))
import           Data.Text.Lazy                    (Text)
import           Data.Word                         (Word8)
import           System.Environment                (getArgs)

rectangle    :: n -> Text -> Dot n
rectangle     = uRectangle []

-- swagger object
mk "rectangle"
   [ ("swagger",             "swagger:str/R")
   , ("info",                "info/R")
   , ("host",                "host:str")
   , ("basePath",            "basePath:str")
   , ("schemes",             "schemes:[str]")
   , ("consumes",            "consumes:[str]")
   , ("produces",            "produces:[str]")
   , ("paths",               "paths/R")
   , ("definitions",         "definitions")
   , ("parameters",          "parameters")
   , ("responses",           "responses")
   , ("securityDefinitions", "securityDefinitions")
   , ("security",            "security")
   , ("tags",                "tags")
   , ("externalDocs",        "externalDocs")
   ]

-- info object
mk "rectangle"
   [ ("iTitle",              "title:str/R")
   , ("iDescription",        "description:str")
   , ("iTermsOfService",     "termOfService:str")
   , ("iContact",            "contact")
   , ("iLicense",            "license")
   , ("iVersion",            "version:str/R")
   ]

-- contact object
mk "rectangle"
   [ ("cName",               "name:str")
   , ("cUrl",                "url:str")
   , ("cEmail",              "email:str")
   ]

-- license object
mk "rectangle"
   [ ("lName",               "name:str/R")
   , ("lUrl",                "url:str")
   ]

-- paths object
mk "rectangle"
   [ ("pPath",               "/{path}")
   ]

-- path item object
mk "rectangle"
   [ ("piRef",               "$ref:str")
   , ("piOperation",         "get,put,post,\ndelete,options\nhead,patch")
   , ("piParameters",        "parameters")
   ]

-- operation object
mk "rectangle"
   [ ("oTags",               "tags:[str]")
   , ("oSummary",            "summary:str")
   , ("oDescription",        "description:str")
   , ("oExternalDocs",       "externalDocs")
   , ("oOperationId",        "operationId:str")
   , ("oConsumes",           "consumes:[str]")
   , ("oProduces",           "produces:[str]")
   , ("oParameters",         "parameters")
   , ("oResponses",          "responses/R")
   , ("oSchemes",            "schemes:[str]")
   , ("oDeprecated",         "deprecated:bool")
   , ("oSecurity",           "security")
   ]

-- external documentation object
mk "rectangle"
   [ ("edDescription",       "description:str")
   , ("edUrl",               "url:str/R")
   ]

-- parameter object
mk "rectangle"
   [ ("prmName",             "name:str/R")
   , ("prmIn",               "query,header,path\nformData,body/R")
   , ("prmDescription",      "description:str")
   , ("prmRequired",         "required:bool/R*in")
   ]

-- NEXT If in is "body"

swagger20 :: G.DotGraph Text
swagger20 = digraph (Str "swagger20") $ do

    graphAttrs [RankDir FromLeft, Compound True]
    cluster (Str "swaggerObject") $ do
        graphAttrs [Label (StrLabel "swagger")]
        swagger; info; host; basePath; schemes; consumes; produces; paths; definitions;
        parameters; responses; securityDefinitions; security; tags; externalDocs;

    cluster (Str "infoObject") $ do
        graphAttrs [Label (StrLabel "info")]
        iTitle; iDescription; iTermsOfService; iContact; iLicense; iVersion

    cluster (Str "contactObject") $ do
        graphAttrs [Label (StrLabel "contact")]
        cName; cUrl; cEmail;

    cluster (Str "licenseObject") $ do
        graphAttrs [Label (StrLabel "license")]
        lName; lUrl;

    cluster (Str "pathsObject") $ do
        graphAttrs [Label (StrLabel "paths")]
        pPath;

    cluster (Str "pathItemObject") $ do
        graphAttrs [Label (StrLabel "path item")]
        piRef; piOperation; piParameters;

    cluster (Str "operationObject") $ do
        graphAttrs [Label (StrLabel "operation")]
        oTags; oSummary; oDescription; oExternalDocs; oOperationId; oConsumes; oProduces; oParameters;
        oResponses; oSchemes; oDeprecated; oSecurity;

    cluster (Str "externalDocumentationObject") $ do
        graphAttrs [Label (StrLabel "external documentation")]
        edDescription; edUrl;

    cluster (Str "parameterObject") $ do
        graphAttrs [Label (StrLabel "parameter")]
        prmName; prmIn; prmDescription; prmRequired;

    edge "info"             "iTitle"        [LHead "cluster_infoObject"]
    edge "iContact"         "cName"         [LHead "cluster_contactObject"]
    edge "iLicense"         "lName"         [LHead "cluster_licenseObject"]

    edge "paths"            "pPath"         [LHead "cluster_pathsObject", textLabel "*"]

    "pPath"            -->  "piRef"
    edge "pPath"            "piOperation"   [textLabel "*"]
    "pPath"            -->  "piParameters"

    edge "piOperation"      "oTags"         [LHead "cluster_operationObject"]
    edge "piParameters"     "prmName"       [LHead "cluster_parameterObject"]
    edge "oExternalDocs"    "edDescription" [LHead "cluster_externalDocumentationObject"]

main :: IO ()
main = do
    as <- getArgs
    let dir = if length as /= 1 then "/tmp" else head as
    doDots dir [ ("swagger20" , swagger20)
               ]
