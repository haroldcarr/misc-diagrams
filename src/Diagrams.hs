{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete hiding (paths)
import           Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                    as L hiding (head, length)
import           Data.Word
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
root, swagger, info, host, basePath, schemes, consumes, produces, paths, definitions,
    parameters, responses, securityDefinitions, security, tags, externalDocs :: Dot L.Text
root                = rectangle    "root"                "root"
swagger             = rectangle    "swagger"             "swagger:str/R"
info                = rectangle    "info"                "info/R"
host                = rectangle    "host"                "host:str"
basePath            = rectangle    "basePath"            "basePath:str"
schemes             = rectangle    "schemes"             "schemes:[str]"
consumes            = rectangle    "consumes"            "consumes:[str]"
produces            = rectangle    "produces"            "produces:[str]"
paths               = rectangle    "paths"               "paths/R"
definitions         = rectangle    "definitions"         "definitions"
parameters          = rectangle    "parameters"          "parameters"
responses           = rectangle    "responses"           "responses"
securityDefinitions = rectangle    "securityDefinitions" "securityDefinitions"
security            = rectangle    "security"            "security"
tags                = rectangle    "tags"                "tags"
externalDocs        = rectangle    "externalDocs"        "externalDocs"

-- info object
iTitle, iDescription, iTermsOfService, iContact, iLicense, iVersion :: Dot L.Text
iTitle              = rectangle    "iTitle"              "title:str/R"
iDescription        = rectangle    "iDescription"        "description:str"
iTermsOfService     = rectangle    "iTermsOfService"     "termOfService:str"
iContact            = rectangle    "iContact"            "contact"
iLicense            = rectangle    "iLicense"            "license"
iVersion            = rectangle    "iVersion"            "version:str/R"

-- contact object
cName, cUrl, cEmail :: Dot L.Text
cName               = rectangle    "cName"               "name:str"
cUrl                = rectangle    "cUrl"                "url:str"
cEmail              = rectangle    "cEmail"              "email:str"

-- license object
lName, lUrl :: Dot L.Text
lName               = rectangle    "lName"               "name:str/R"
lUrl                = rectangle    "lUrl"                "url:str"

-- paths object
pPath :: Dot L.Text
pPath               = rectangle    "pPath"               "/{path}"

-- path item object
piRef, piOperation, piParameters :: Dot L.Text
piRef               = rectangle    "piRef"               "$ref:str"
piOperation         = rectangle    "piOperation"         "get,put,post,\ndelete,options\nhead,patch"
piParameters        = rectangle    "piParameters"        "parameters"

-- operation object
oTags, oSummary, oDescription, oExternalDocs, oOperationId, oConsumes, oProduces, oParameters,
    oResponses, oSchemes, oDeprecated, oSecurity :: Dot L.Text
oTags               = rectangle    "oTags"               "tags:[str]"
oSummary            = rectangle    "oSummary"            "summary:str"
oDescription        = rectangle    "oDescription"        "description:str"
oExternalDocs       = rectangle    "oExternalDocs"       "externalDocs"
oOperationId        = rectangle    "oOperationId"        "operationId:str"
oConsumes           = rectangle    "oConsumes"           "consumes:[str]"
oProduces           = rectangle    "oProduces"           "produces:[str]"
oParameters         = rectangle    "oParameters"         "parameters"
oResponses          = rectangle    "oResponses"          "responses/R"
oSchemes            = rectangle    "oSchemes"            "schemes:[str]"
oDeprecated         = rectangle    "oDeprecated"         "deprecated:bool"
oSecurity           = rectangle    "oSecurity"           "security"

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
