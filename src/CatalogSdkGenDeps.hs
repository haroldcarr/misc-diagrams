{-
Created       : 2014 Jul 24 (Thu) 09:37:09 by Harold Carr.
Last Modified : 2017 Jun 16 (Fri) 10:31:39 by Harold Carr.
-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Data.GraphViz                     (GraphID (Str), GraphvizCommand (Fdp, Fdp, TwoPi))
import           Data.GraphViz.Attributes.Complete (Attribute (RankDir),
                                                    RankDir (FromLeft))
import           Data.GraphViz.HC.Util             (doDots', uRectangle)
import           Data.GraphViz.Types.Generalised   as G (DotGraph)
import           Data.GraphViz.Types.Monadic       (digraph, graphAttrs, (-->))
import           Data.Text.Lazy                    as L (Text)

{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

------------------------------------------------------------------------------

f x = uRectangle     [] x x

------------------------------------------------------------------------------


catalogSdkGenDeps :: G.DotGraph L.Text
catalogSdkGenDeps = digraph (Str "catalog-sdk-gen-deps") $ do

--    graphAttrs [RankDir FromLeft]

"opc-sdk-java-apicatalog:commitAndPushApicatalogApisAndSDKs" --> "opc-sdk-java-apicatalog:syncApicatalogApisAndSDKs"
"opc-sdk-java-apicatalog:commitAndPushApisAndSDKs" --> "opc-sdk-java-apicatalog:commitAndPushApicatalogApisAndSDKs"

"opc-sdk-java-apicatalog:commitAndPushConfigs"

"opc-sdk-java-apicatalog:compileApicatalogSDKs" --> "opc-sdk-java-apicatalog:generateApicatalogSDKs"
"opc-sdk-java-apicatalog:compileApicatalogSDKTests" --> "opc-sdk-java-apicatalog:compileApicatalogSDKs"
"opc-sdk-java-apicatalog:compileSDKs" --> "opc-sdk-java-apicatalog:compileApicatalogSDKs"
"opc-sdk-java-apicatalog:compileSDKTests" --> "opc-sdk-java-apicatalog:compileApicatalogSDKTests"

"opc-sdk-java-apicatalog:configureApiCollectionApicatalog"

"opc-sdk-java-apicatalog:configureApiCollections" --> "opc-sdk-java-apicatalog:configureApiCollectionApicatalog"
"opc-sdk-java-apicatalog:generateApicatalogAPI CollectionsSDKs" --> "opc-sdk-java-apicatalog:populateApicatalogAPI CollectionsApis"
"opc-sdk-java-apicatalog:generateApicatalogCategoriesSDKs" --> "opc-sdk-java-apicatalog:populateApicatalogCategoriesApis"
"opc-sdk-java-apicatalog:generateApicatalogOrganizationsSDKs" --> "opc-sdk-java-apicatalog:populateApicatalogOrganizationsApis"

"opc-sdk-java-apicatalog:generateApicatalogSDKs" --> "opc-sdk-java-apicatalog:generateApicatalogAPI CollectionsSDKs, opc-sdk-java-apicatalog:generateApicatalogCategoriesSDKs, opc-sdk-java-apicatalog:generateApicatalogOrganizationsSDKs, opc-sdk-java-apicatalog:generateApicatalogSearchSDKs"

"opc-sdk-java-apicatalog:generateApicatalogSearchSDKs" --> "opc-sdk-java-apicatalog:populateApicatalogSearchApis"
"opc-sdk-java-apicatalog:generateSDKs" --> "opc-sdk-java-apicatalog:generateApicatalogSDKs"

"opc-sdk-java-apicatalog:populateApicatalogAPI CollectionsApis"

"opc-sdk-java-apicatalog:populateApicatalogApis" --> "opc-sdk-java-apicatalog:populateApicatalogAPI CollectionsApis, opc-sdk-java-apicatalog:populateApicatalogCategoriesApis, opc-sdk-java-apicatalog:populateApicatalogOrganizationsApis, opc-sdk-java-apicatalog:populateApicatalogSearchApis"

"opc-sdk-java-apicatalog:populateApicatalogCategoriesApis"
"opc-sdk-java-apicatalog:populateApicatalogOrganizationsApis"
"opc-sdk-java-apicatalog:populateApicatalogSearchApis"

"opc-sdk-java-apicatalog:populateApis" --> "opc-sdk-java-apicatalog:populateApicatalogApis"

"opc-sdk-java-apicatalog:syncApicatalogApis" --> "opc-sdk-java-apicatalog:generateApicatalogSDKs"

"opc-sdk-java-apicatalog:syncApicatalogApisAndSDKs" --> "opc-sdk-java-apicatalog:syncApicatalogApis, opc-sdk-java-apicatalog:syncApicatalogSDKs"

"opc-sdk-java-apicatalog:syncApicatalogSDKs" --> "opc-sdk-java-apicatalog:generateApicatalogSDKs"
"opc-sdk-java-apicatalog:syncApisAndSDKs" --> "opc-sdk-java-apicatalog:syncApicatalogApisAndSDKs"

"opc-sdk-java-apicatalog:testApicatalogSDKs" --> " [opc-sdk-java-apicatalog:compileApicatalogSDKTests"
"opc-sdk-java-apicatalog:testSDKs" --> "opc-sdk-java-apicatalog:testApicatalogSDKs"

------------------------------------------------------------------------------

main :: IO ()
main =
    doDots' "/tmp" Fdp -- TwoPi -- Fdp
            [ ("catalog-sdk-gen-deps" , catalogSdkGenDeps)
            ]

-- End of file.
