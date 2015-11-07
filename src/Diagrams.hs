{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete hiding (paths)
import           Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                    as L hiding (head, length,
                                                         map)
import           Data.Word
import           DiagramsTH
import           System.Environment                (getArgs)
import           WriteRunDot

-- http://www.colorcombos.com/color-schemes/2025/ColorCombo2025.html
myColorCL :: Word8 -> ColorList
myColorCL n | n == 1 = c (RGB 127 108 138)
            | n == 2 = c (RGB 175 177 112)
            | n == 3 = c (RGB 226 206 179)
            | n == 4 = c (RGB 172 126 100)
 where c rgb = toColorList [rgb]

myColor :: Word8 -> Attribute
myColor n = Color $ myColorCL n

doubleCircle :: n -> Text -> Dot n
doubleCircle n l = node n [textLabel l, shape DoubleCircle, FixedSize SetNodeSize, Width 1, style filled, myColor 1]

circle       :: n -> Text -> Dot n
circle       n l = node n [textLabel l, shape       Circle, FixedSize SetNodeSize, Width 1, style filled, myColor 1]

rectangle    :: n -> Text -> Dot n
rectangle    n l = node n [textLabel l, shape     BoxShape,                 Width 1, style filled, myColor 3]

-- swagger object
mk [ ("root",                "root")
   , ("swagger",             "swagger:str/R")
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
mk [ ("iTitle",              "title:str/R")
   , ("iDescription",        "description:str")
   , ("iTermsOfService",     "termOfService:str")
   , ("iContact",            "contact")
   , ("iLicense",            "license")
   , ("iVersion",            "version:str/R")
   ]

-- contact object
mk [ ("cName",               "name:str")
   , ("cUrl",                "url:str")
   , ("cEmail",              "email:str")
   ]

-- license object
mk [ ("lName",               "name:str/R")
   , ("lUrl",                "url:str")
   ]

-- paths object
mk [ ("pPath",               "/{path}")
   ]

-- path item object
mk [ ("piRef",               "$ref:str")
   , ("piOperation",         "get,put,post,\ndelete,options\nhead,patch")
   , ("piParameters",        "parameters")
   ]

-- operation object
mk [ ("oTags",               "tags:[str]")
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

-- NEXT external documentation object

swagger20 :: G.DotGraph L.Text
swagger20 = digraph (Str "swagger20") $ do

    graphAttrs [RankDir FromLeft]
    root; swagger; info; host; basePath; schemes; consumes; produces; paths; definitions;
    parameters; responses; securityDefinitions; security; tags; externalDocs;

    iTitle; iDescription; iTermsOfService; iContact; iLicense; iVersion

    cName; cUrl; cEmail;

    lName; lUrl;

    pPath;

    piRef; piOperation; piParameters;

    oTags; oSummary; oDescription; oExternalDocs; oOperationId; oConsumes; oProduces; oParameters;
    oResponses; oSchemes; oDeprecated; oSecurity;

    "root"             --> "swagger"
    "root"             --> "info"
    "root"             --> "host"
    "root"             --> "basePath"
    "root"             --> "schemes"
    "root"             --> "consumes"
    "root"             --> "produces"
    "root"             --> "paths"
    "root"             --> "definitions"
    "root"             --> "parameters"
    "root"             --> "responses"
    "root"             --> "securityDefinitions"
    "root"             --> "security"
    "root"             --> "tags"
    "root"             --> "externalDocs"

    "info"             --> "iTitle"
    "info"             --> "iDescription"
    "info"             --> "iTermsOfService"
    "info"             --> "iContact"
    "info"             --> "iLicense"
    "info"             --> "iVersion"

    "iContact"         --> "cName"
    "iContact"         --> "cUrl"
    "iContact"         --> "cEmail"

    "iLicense"         --> "lName"
    "iLicense"         --> "lUrl"

    edge "paths"           "pPath"         [textLabel "*"]

    "pPath"            --> "piRef"
    edge "pPath"           "piOperation"   [textLabel "*"]
    "pPath"            --> "piParameters"

    "piOperation"      --> "oTags"
    "piOperation"      --> "oSummary"
    "piOperation"      --> "oDescription"
    "piOperation"      --> "oExternalDocs"
    "piOperation"      --> "oOperationId"
    "piOperation"      --> "oConsumes"
    "piOperation"      --> "oProduces"
    "piOperation"      --> "oParameters"
    "piOperation"      --> "oResponses"
    "piOperation"      --> "oSchemes"
    "piOperation"      --> "oDeprecated"
    "piOperation"      --> "oSecurity"

main :: IO ()
main = do
    as <- getArgs
    let dir = if length as /= 1 then "/tmp" else head as
    doDots dir [ ("swagger20" , swagger20)
               ]
