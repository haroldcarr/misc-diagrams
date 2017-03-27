{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.GraphViz                     (GraphID (Str),
                                                    Shape (BoxShape), filled,
                                                    shape, style, textLabel)
import           Data.GraphViz.Attributes.Complete (Attribute (Color, Compound, FixedSize, LHead, Label, NodeSep, RankDir, RankSep, Width),
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
import           Text.RawString.QQ

rectangle    :: n -> Text -> Dot n
rectangle     = uRectangle []

-- swagger object
mk "rectangle"
   -------------------------
   -- domain/registrar
   [ ("many"
     ,[r|discreetclothing.com
discreetheadwear.com
discreteapparel.com
discreteapparel.net
discreteattire.com
discreteclothes.com
discreteclothing.net
discreteclothingco.com
discreteclothingusa.com
discreteco.com
discretehats.com
discretehats.net
discreteheadwear.net
discreteltd.com
discreteofficial.com
discretesport.net
discretethreads.com
discreteusa.com
discreteworld.com|])
   , ("discreteclothing_com"
     ,"++discreteclothing.com")
   , ("discreteheadwear_com"
     ,"++discreteheadwear.com")
   , ("discretesport_com"
     ,"discretesport.com")
   , ("juliancarr_com"
     ,"+juliancarr.com")
   , ("juliancarr_net"
     ,"juliancarr.net")
   , ("peak_series_com"
     ,"+peak-series.com")
   , ("peaks"
     ,[r|+pushpeaks.com
peakseriesraces.com
peakseries.net
discreteseries.com|])
   , ("rockdiscrete_com"
     ,"+rockdiscrete.com")
   -------------------------
   -- nameserver
   , ("discretehats_nfshost_com"
     ,[r|* A 208.94.116.204
* TXT v=spf1 -all
www.* CNAME discretehats.nfshost.com.|])

   , ("discreteclothing_com_NFS_NS"
     ,[r|* A 208.94.116.204
* www.* CNAME	discretehats.nfshost.com.
* MX 1 ASPMX.L.GOOGLE.COM.
* MX 5 ALT1.ASPMX.L.GOOGLE.COM.
* MX 5 ALT2.ASPMX.L.GOOGLE.COM.
* MX 10 ASPMX2.GOOGLEMAIL.COM.
* MX 10 ASPMX3.GOOGLEMAIL.COM.
* TXT google-site-verification=fEDSdJrQYbdSVuHny01j4NH7ioyxZzMnTxMDSBjxyz0|])
   , ("discreteclothing_com_GoDadday_NS"
     ,[r|NS[01|02]_DOMAINCONTROL_COM
nslookup DISCRETECLOTHING.COM:
23.227.38.32 (shopify)|])

   , ("ns_phx12_nearlyfreespeech_net"
     ,[r|ns.phx[1|2].nearlyfreespeech.net.
discreteheadwear.com A 208.94.116.204
discreteheadwear.com MX 0 mx-fwd-1.nearlyfreespeech.net.
shop.discreteheadwear.com CNAME discreteheadwear-com.myshopify.com.
www.discreteheadwear.com A 23.227.38.32|])

   , ("discretesport_com_NS"
     ,"discretesport.com NS")
   , ("juliancarr_nfshost_com"
     ,[r|juliancarr.com A 208.94.116.204
* TXT v=spf1 -all
www.juliancarr.com CNAME juliancarr.nfshost.com.|])
   , ("juliancarr_net_NS"
     ,"juliancarr.net NS")

   , ("peak_series_nfshost_com"
     ,[r|peak-series.com A 208.94.118.220
www.peak-series.com CNAME peak-series.nfshost.com.|])

   , ("peaks_NS"
     ,"* A 208.94.118.220")

   , ("rockdiscrete_NS"
     ,[r|rockdiscrete.combination A 208-94-116-38
www.rockdiscrete.com CNAME discretehats.nfshost.com.|])
   -------------------------
   -- site
   , ("site_discretehats"
     ,[r|discretehats.nfshost.com
IP 208.94.116.38
.htaccess Redirect
/ http://www.discreteheadwear.com|])

   , ("shopify"
     ,[r|shopify
23.227.38.32|])

   , ("site_juliancarr"
     ,[r|juliancarr.nfshost.com
IP 208.94.118.148
.htaccess Redirect
/ https://juliancarr.squarespace.com/|])
   , ("site_juliancarr_squarespace"
     ,"https://juliancarr.squarespace.com/")

   , ("site_peak_series"
     ,[r|peak-series.nfshost.com
IP 208.94.118.220
.htaccess Redirect
/ http://rock.discreteclothing.com/peak-series/|])
   -------------------------
   -- mail, etc
   , ("mx_google"
     ,"MX google")
   , ("discreteheadwear_com_mail_forwarding"
     ,"forwarding")
   , ("discretesport_com_mail_forwarding"
     ,"forwarding")
   , ("juliancarr_net_mail_forwarding"
     ,"forwarding")
   ]

discrete :: G.DotGraph Text
discrete = digraph (Str "discrete") $ do

    graphAttrs [RankDir FromLeft, Compound True, NodeSep 0.5, RankSep [3.0]]
    cluster (Str "registarNFSCluster") $ do
        graphAttrs [Label (StrLabel "registrar NFS")]
        many; discretesport_com; juliancarr_com; juliancarr_net; peak_series_com; peaks; rockdiscrete_com

    cluster (Str "registrarGoDaddyCluster") $ do
        graphAttrs [Label (StrLabel "registrar GoDaddy")]
        discreteclothing_com; discreteheadwear_com

    cluster (Str "nameserverNFSCluster") $ do
        graphAttrs [Label (StrLabel "nameserver NFS")]
        discreteclothing_com_NFS_NS
        discretehats_nfshost_com; ns_phx12_nearlyfreespeech_net;
        discretesport_com_NS;
        juliancarr_nfshost_com; juliancarr_net_NS;
        peak_series_nfshost_com;
        peaks_NS; rockdiscrete_NS

    cluster (Str "nameserverGoDaddyCluster") $ do
        graphAttrs [Label (StrLabel "nameserver GoDaddy")]
        discreteclothing_com_GoDadday_NS

    cluster (Str "siteNFS") $ do
        graphAttrs [Label (StrLabel "site NFS")]
        site_discretehats; site_juliancarr; site_peak_series

    cluster (Str "mailNFS") $ do
        graphAttrs [Label (StrLabel "mail NFS")]
        discreteheadwear_com_mail_forwarding;
        discretesport_com_mail_forwarding;
        juliancarr_net_mail_forwarding;

    mx_google
    shopify
    site_juliancarr_squarespace

    --------------------------------------------------
    -- registrar               --> nameserver
    "discreteclothing_com"     --> "discreteclothing_com_GoDadday_NS"
    "discreteclothing_com"     --> "discreteclothing_com_NFS_NS"
    "discreteheadwear_com"     --> "ns_phx12_nearlyfreespeech_net"
    "ns_phx12_nearlyfreespeech_net"
                               --> "shopify"
    "discretesport_com"        --> "discretehats_nfshost_com"
    "discretesport_com"        --> "discretesport_com_NS"
    "juliancarr_com"           --> "juliancarr_nfshost_com"
    "juliancarr_net"           --> "juliancarr_nfshost_com"
    "juliancarr_net"           --> "juliancarr_net_NS"
    "peak_series_com"          --> "peak_series_nfshost_com"
    "peaks"                    --> "peaks_NS"
    "rockdiscrete_com"         --> "rockdiscrete_NS"

    "many"                     --> "discretehats_nfshost_com"
    --------------------------------------------------
    -- nameserver              --> site
    "discreteclothing_com_NFS_NS"
                               --> "site_discretehats"
    "discretehats_nfshost_com" --> "site_discretehats"
    "site_discretehats"        --> "ns_phx12_nearlyfreespeech_net"
    "discreteclothing_com_GoDadday_NS"
                               --> "shopify"
    "juliancarr_nfshost_com"   --> "site_juliancarr"
    "site_juliancarr"          --> "site_juliancarr_squarespace"
    "peak_series_nfshost_com"  --> "site_peak_series"
    "site_peak_series"         --> "discreteclothing_com_GoDadday_NS"
    "peaks_NS"                 --> "site_peak_series"
    "rockdiscrete_NS"          --> "site_discretehats"
    --------------------------------------------------
    -- nameserver              --> mail/etc
    "discreteclothing_com_NFS_NS"
                               --> "mx_google"
    "ns_phx12_nearlyfreespeech_net"
                               --> "discreteheadwear_com_mail_forwarding"
    "discretesport_com_NS"     --> "discretesport_com_mail_forwarding"
    "juliancarr_net_NS"        --> "juliancarr_net_mail_forwarding"

main :: IO ()
main = do
    as <- getArgs
    let dir = if length as /= 1 then "/tmp" else head as
    doDots dir [ ("discrete" , discrete)
               ]
