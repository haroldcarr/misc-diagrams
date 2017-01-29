{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.GraphViz                     (GraphID (Str),
                                                    Shape (BoxShape), filled,
                                                    shape, style, textLabel)
import           Data.GraphViz.Attributes.Complete (Attribute (Color, Compound, FixedSize, LHead, Label, RankDir, Width),
                                                    Color (RGB), ColorList (..),
                                                    Label (StrLabel),
                                                    NodeSize (SetNodeSize),
                                                    RankDir (FromLeft),
                                                    toColorList)
import           Data.GraphViz.HC.DiagramsTH       (mk)
import           Data.GraphViz.HC.Util             (colorCombo2025, doDots,
                                                    uRectangle)
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
   -------------------------
   -- domain/registrar
   [ ("discreetclothing_com"
     ,"discreetclothing.com")
   , ("discreetheadwear_com"
     ,"discreetheadwear.com")
   , ("discreteapparel_com"
     ,"discreteapparel.com")
   , ("discreteapparel_net"
     ,"discreteapparel.net")
   , ("discreteattire_com"
     ,"discreteattire.com")
   , ("discreteclothes_com"
     ,"discreteclothes.com")
   , ("discreteclothing_com"
     ,"discreteclothing.com")
   , ("discreteclothing_net"
     ,"discreteclothing.net")
   , ("discreteclothingco_com"
     ,"discreteclothingco.com")
   , ("discreteclothingusa_com"
     ,"discreteclothingusa.com")
   , ("discreteco_com"
     ,"discreteco.com")
   , ("discretefashion_com"
     ,"discretefashion.com")
   , ("discretehats_net"
     ,"discretehats.net")
   , ("discreteheadwear_com"
     ,"discreteheadwear.com")
   , ("discreteheadwear_net"
     ,"discreteheadwear.net")
   , ("discreteltd_com"
     ,"discreteltd.com")
   , ("discreteofficial_com"
     ,"discreteofficial.com")
   , ("discretesport_com"
     ,"discretesport.com")
   , ("discretesport_net"
     ,"discretesport.net")
   , ("discretethreads_com"
     ,"discretethreads.com")
   , ("discreteusa_com"
     ,"discreteusa.com")
   , ("discreteworld_com"
     ,"discreteworld.com")
   , ("juliancarr_com"
     ,"juliancarr.com")
   , ("juliancarr_net"
     ,"juliancarr.net")
   , ("peak_series_com"
     ,"peak-series.com")
   , ("pushpeaks_com"
     ,"pushpeaks.com")
   , ("rockdiscrete_com"
     ,"rockdiscrete.com")
   -------------------------
   -- nameserver
   , ("discretehats_nfshost_com"
     ,"* A 208.94.116.204\n*TXT v=spf1 -all\nwww.* CNAME discretehats.nfshost.com.")

   , ("ns0102_domaincontrol_com"
     ,"NS[01|02]_DOMAINCONTROL_COM\nnslookup DISCRETECLOTHING.COM: 23.227.38.32 (shopify)")

   , ("ns_phx12_nearlyfreespeech_net"
     ,"ns.phx[1|2].nearlyfreespeech.net.\ndiscreteheadwear.com A 208.94.116.204\ndiscreteheadwear.com MX 0 mx-fwd-1.nearlyfreespeech.net.\nshop.discreteheadwear.com CNAME discreteheadwear-com.myshopify.com.\nwww.discreteheadwear.com A 23.227.38.32")

   , ("juliancarr_nfshost_com"
     ,"juliancarr.com A 208.94.116.204\n*TXT v=spf1 -all\nwww.juliancarr.com CNAME juliancarr.nfshost.com.")

   , ("peak_series_nfshost_com"
     ,"peak-series.com A 208.94.118.220\nwww.peak-series.com CNAME peak-series.nfshost.com." )

   , ("pushpeaks_NS"
     ,"pushpeaks.com A 208.94.118.220")

   , ("rockdiscrete_NS"
     ,"no A nor CNAME record")

   -------------------------
   -- site
   , ("site_discretehats"
     ,"discretehats.nfshost.com\nIP 208.94.116.38\n.htaccess Redirect\n/ http://www.discreteheadwear.com")

   , ("shopify"
     ,"shopify\n23.227.38.32")

   , ("site_juliancarr"
     ,"site NFS\ndiscretehats.nfshost.com\nIP 208.94.118.148\n.htaccess Redirect\n/ https://juliancarr.squarespace.com/")
   , ("site_juliancarr_squarespace"
     ,"https://juliancarr.squarespace.com/")

   , ("site_peak_series"
     ,"peak-series.nfshost.com\nIP 208.94.118.220\n.htaccess Redirect / http://rock.discreteclothing.com/peak-series/")
   ]

discrete :: G.DotGraph Text
discrete = digraph (Str "discrete") $ do

    graphAttrs [RankDir FromLeft, Compound True]
    cluster (Str "registarNFSCluster") $ do
        graphAttrs [Label (StrLabel "registrar NFS")]
        discreetclothing_com; discreetheadwear_com; discreteapparel_com; discreteapparel_net; discreteattire_com
        discreteclothes_com; discreteclothing_net; discreteclothingco_com; discreteclothingusa_com;
        discreteco_com; discretefashion_com; discretehats_net; discreteheadwear_net; discreteltd_com;
        discreteofficial_com; discretesport_com; discretesport_net; discretethreads_com; discreteusa_com;
        discreteworld_com; juliancarr_com; juliancarr_net; peak_series_com; pushpeaks_com; rockdiscrete_com

    cluster (Str "registrarGoDaddyCluster") $ do
        graphAttrs [Label (StrLabel "registrar GoDaddy")]
        discreteclothing_com; discreteheadwear_com

    cluster (Str "nameserverNFSCluster") $ do
        graphAttrs [Label (StrLabel "nameserver NFS")]
        discretehats_nfshost_com; ns_phx12_nearlyfreespeech_net; juliancarr_nfshost_com; peak_series_nfshost_com;
        pushpeaks_NS; rockdiscrete_NS

    cluster (Str "nameserverGoDaddyCluster") $ do
        graphAttrs [Label (StrLabel "nameserver GoDaddy")]
        ns0102_domaincontrol_com

    cluster (Str "siteNFS") $ do
        graphAttrs [Label (StrLabel "site NFS")]
        site_discretehats; site_juliancarr; site_peak_series

    shopify
    site_juliancarr_squarespace

    --------------------------------------------------
    -- registrar               --> nameserver
    "discreetclothing_com"     --> "discretehats_nfshost_com"
    "discreetheadwear_com"     --> "discretehats_nfshost_com"
    "discreteapparel_com"      --> "discretehats_nfshost_com"
    "discreteapparel_net"      --> "discretehats_nfshost_com"
    "discreteattire_com"       --> "discretehats_nfshost_com"
    "discreteclothes_com"      --> "discretehats_nfshost_com"
    "discreteclothing_com"     --> "ns0102_domaincontrol_com"
    "discreteclothing_net"     --> "discretehats_nfshost_com"
    "discreteclothingco_com"   --> "discretehats_nfshost_com"
    "discreteclothingusa_com"  --> "discretehats_nfshost_com"
    "discreteco_com"           --> "discretehats_nfshost_com"
    "discretefashion_com"      --> "discretehats_nfshost_com"
    "discretehats_net"         --> "discretehats_nfshost_com"
    "discreteheadwear_com"     --> "ns_phx12_nearlyfreespeech_net"
    "ns_phx12_nearlyfreespeech_net"
                               --> "shopify"
    "discreteheadwear_net"     --> "discretehats_nfshost_com"
    "discreteltd_com"          --> "discretehats_nfshost_com"
    "discreteofficial_com"     --> "discretehats_nfshost_com"
    "discretesport_com"        --> "discretehats_nfshost_com"
    "discretesport_net"        --> "discretehats_nfshost_com"
    "discretethreads_com"      --> "discretehats_nfshost_com"
    "discreteusa_com"          --> "discretehats_nfshost_com"
    "discreteworld_com"        --> "discretehats_nfshost_com"
    "juliancarr_com"           --> "juliancarr_nfshost_com"
    "juliancarr_net"           --> "juliancarr_nfshost_com"
    "peak_series_com"          --> "peak_series_nfshost_com"
    "pushpeaks_com"            --> "pushpeaks_NS"
    "rockdiscrete_com"         --> "rockdiscrete_NS"
    --------------------------------------------------
    -- nameserver              --> site
    "discretehats_nfshost_com" --> "site_discretehats"
    "site_discretehats"        --> "ns_phx12_nearlyfreespeech_net"
    "ns0102_domaincontrol_com" --> "shopify"
    "juliancarr_nfshost_com"   --> "site_juliancarr"
    "site_juliancarr"          --> "site_juliancarr_squarespace"
    "peak_series_nfshost_com"  --> "site_peak_series"
    "site_peak_series"         --> "ns0102_domaincontrol_com"
    "pushpeaks_NS"             --> "site_peak_series"

main :: IO ()
main = do
    as <- getArgs
    let dir = if length as /= 1 then "/tmp" else head as
    doDots dir [ ("discrete" , discrete)
               ]
