{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-
Created       : 2014 Feb 26 (Wed) 18:54:30 by Harold Carr.
Last Modified : 2015 Nov 11 (Wed) 20:32:05 by Harold Carr.
-}

module Main where

import           Data.GraphViz                          (filled, shape, style,
                                                         textLabel)
import           Data.GraphViz.Attributes.Colors.Brewer (BrewerColor (BC),
                                                         BrewerName (Pastel2),
                                                         BrewerScheme (BScheme))
import           Data.GraphViz.Attributes.Complete      (Attribute (Color, FixedSize, Height, RankDir, Width),
                                                         NodeSize (SetNodeSize),
                                                         Number (Int),
                                                         RankDir (FromLeft), Shape (Circle, BoxShape, DiamondShape, DoubleCircle),
                                                         toColor, toColorList)
import           Data.GraphViz.HC.Util                  (doDots, pastel28)
import qualified Data.GraphViz.Types.Generalised        as G (DotGraph)
import           Data.GraphViz.Types.Monadic            (Dot,
                                                         GraphID (Str, Num),
                                                         cluster, digraph, edge,
                                                         graphAttrs, node,
                                                         (-->))
import qualified Data.Text                              as T (Text)
import           Data.Text.Lazy                         as L (Text)
import           Data.Word                              (Word8)
default (T.Text)

------------------------------------------------------------------------------
-- DIAGRAM HELPER FUNCTIONS

startEndClosedState                 :: L.Text -> L.Text -> Dot L.Text
startEndClosedState n l             = node n [textLabel l, shape DoubleCircle, pastel28 1, style filled, FixedSize SetNodeSize, Width 1]

state                               :: L.Text -> L.Text -> Dot L.Text
state               n l             = node n [textLabel l, shape       Circle, pastel28 2, style filled, FixedSize SetNodeSize, Width 1]

transition                          :: L.Text -> L.Text -> Dot L.Text
transition          n l             = node n [textLabel l, shape     BoxShape, pastel28 5, style filled]

decision                            :: L.Text -> L.Text -> Dot L.Text
decision            n l             = node n [textLabel l, shape DiamondShape, pastel28 6, style filled, FixedSize SetNodeSize, Width 1.5, Height 1.5]

------------------------------------------------------------------------------
-- STATES

closedStart                         :: Dot L.Text
closedStart                         = startEndClosedState "ClosedStart"              "L000\nclosed"
closedEnd                           :: Dot L.Text
closedEnd                           = startEndClosedState "ClosedEnd"                "R000\nclosed"
open                                :: Dot L.Text
open                                = state               "Open"                     "200\nopen"
openExpectDATA                      :: Dot L.Text
openExpectDATA                      = state               "OpenExpectDATA"           "400\nopen expect\ndata"
halfClosed                          :: Dot L.Text
halfClosed                          = state               "HalfClosed"               "700\nhalf-clsd"

-----

graceFulSD                          :: Dot L.Text
graceFulSD                          = state               "GraceFulSD"               "graceful\nshutdown"

graceFulSDAck                       :: Dot L.Text
graceFulSDAck                       = state               "GraceFulSDAck"            "graceful\nshutdown\nacked"

-----

connected                           :: Dot L.Text
connected                           = state               "Connected"                "connected"

initialized                         :: Dot L.Text
initialized                         = state               "Initialized"              "initialized"

released                            :: Dot L.Text
released                            = state               "Released"                 "released"

openConnection                      :: Dot L.Text
openConnection                      = state               "OpenConnection"           "open\nconnection"

------------------------------------------------------------------------------
-- TRANSITIONS

requestDATA                         :: Dot L.Text
requestDATA                         = transition          "requestDATA"              "500\nrequest\nDATA"
responseDATA                        :: Dot L.Text
responseDATA                        = transition          "responseDATA"             "800\nresponse\nDATA"
startMessage                        :: Dot L.Text
startMessage                        = transition          "startMessage"             "100\nrequest\nSTART_MSG"

queuesDrained                       :: Dot L.Text
queuesDrained                       = transition          "queuesDrained"                 "queues\ndrained"

ignoreAnyFrameExceptDataForExisting :: Dot L.Text
ignoreAnyFrameExceptDataForExisting = transition          "ignoreAnyFrameExceptDataForExisting"    "ignore\nany frames except\ndata for existing\nmessages"

normalProcessing                    :: Dot L.Text
normalProcessing                    = transition          "normalProcessing"        "normal\nprocessing"

-----

initOne                             :: Dot L.Text
initOne                             = transition          "initOne"                  "INIT"

initAfterInit                       :: Dot L.Text
initAfterInit                       = transition          "initAfterInit"            "INIT"

receiptOrTimeout                    :: Dot L.Text
receiptOrTimeout                    = transition          "receiptOrTimeout"        "RECEIPT\nor timeout"

releaseConnection                   :: Dot L.Text
releaseConnection                   = transition          "releaseConnection"        "release\nconnection"

nonInitFrame                        :: Dot L.Text
nonInitFrame                        = transition          "nonInitFrame"             "non-INIT frame,\nnot version 1 format,\ncannot decode"

gracefulShutdown                    :: Dot L.Text
gracefulShutdown                    = transition          "gracefulShutdown"         "GRACEFUL_SHUTDOWN\nNO_ERROR"

gracefulShutdownAck                 :: Dot L.Text
gracefulShutdownAck                 = transition        "gracefulShutdownAck"      "GRACEFUL_SHUTDOWN\nGRACEFUL_SHUTDOWN_ACK"

gracefulShutdownAckTimeout          :: Dot L.Text
gracefulShutdownAckTimeout          = transition        "gracefulShutdownAckTimeout"      "GRACEFUL_SHUTDOWN_ACK\nTIMEOUT"

startMessageIdN                     :: Dot L.Text
startMessageIdN                     = transition          "startMessageIdN"          "START Id=N"

unknownFrameType                    :: Dot L.Text
unknownFrameType                    = transition          "unknownFrameType"         "Unknown Frame Type\nId=N"

rstUnknownFrameType                 :: Dot L.Text
rstUnknownFrameType                 = transition        "rstUnknownFrameType"      "RST Id=N\nUNKNOWN_FRAME_TYPE"

startId0                            :: Dot L.Text
startId0                            = transition          "startId0"                 "START Id=0"
rstStartId0                         :: Dot L.Text
rstStartId0                         = transition          "rstStartId0"              "RST Id=0\nSTART_WITH_ZERO_ID"

dataId0                             :: Dot L.Text
dataId0                             = transition          "dataId0"                  "DATA Id=0"
rstDataId0                          :: Dot L.Text
rstDataId0                          = transition          "rstDataId0"               "RST Id=0\nDATA_WITH_ZERO_ID"

dataOnClosed                        :: Dot L.Text
dataOnClosed                        = transition          "dataOnClosed"           "DATA\nId=N"

rstDataOnClosed                     :: Dot L.Text
rstDataOnClosed                     = transition          "rstDataOnClosed"          "RST Id=N\nDATA_ON_CLOSED_ID"

rstOnClosed                         :: Dot L.Text
rstOnClosed                         = transition          "rstOnClosed"              "RST\nId=N"

rstOpenOrHalfClosed                 :: Dot L.Text
rstOpenOrHalfClosed                 = transition          "rstOpenOrHalfClosed"  "RST Id=N\nCLOSE_ID"

startOnNonClosed                    :: Dot L.Text
startOnNonClosed                    = transition          "startOnNonClosed"  "START Id=N"

rstStartOnNonClosed                 :: Dot L.Text
rstStartOnNonClosed                 = transition          "rstStartOnNonClosed"  "RST Id=N\nSTART_ON_NON_CLOSED_ID"

------------------------------------------------------------------------------
-- DECISIONS

requestEndMessageP                  :: Dot L.Text
requestEndMessageP                  = decision         "RequestEndMessageP"       "300 request\nend message?"
requestEndMessageP2                 :: Dot L.Text
requestEndMessageP2                 = decision         "RequestEndMessageP2"      "600 request\nend message?"
responseEndMessageP                 :: Dot L.Text
responseEndMessageP                 = decision         "ResponseEndMessageP"      "900 response\nend message?"
requestOneWayMessageP               :: Dot L.Text
requestOneWayMessageP               = decision         "RequestOneWayMessageP" "650 request\none-way\nmessage?"

------------------------------------------------------------------------------
-- IMPLEMENTATION DIAGRAMS

--------------------------------------------------
-- top-level impl architecture

sendGetId :: Dot L.Text
sendGetId = startEndClosedState "sendGetId" "send\ngetId"

epToConnectionInfo :: Dot L.Text
epToConnectionInfo = transition "epToConnectionInfo" "epToConnection\nInfo"

connectionInfo :: Dot L.Text
connectionInfo = transition "connectionInfo" "Connection\nInfo"

atomicLong :: Dot L.Text
atomicLong = transition "atomicLong" "AtomicLong"

ourIdToMessageMap :: Dot L.Text
ourIdToMessageMap = transition "ourIdToMessageMap" "ourIdTo\nMessageMap"

theirIdToMessageMap :: Dot L.Text
theirIdToMessageMap = transition "theirIdToMessageMap" "theirIdTo\nMessageMap"

messageInfo :: Dot L.Text
messageInfo = transition "messageInfo" "MessageInfo\n-----------\nmessageId\nBuzzMessageState\n\nList<BufferSequence>"

subprotocolHandlers :: Dot L.Text
subprotocolHandlers = transition "subprotocolHandlers" "subprotocol\nHandlers"

subprotocolHandler :: Dot L.Text
subprotocolHandler = transition "subprotocolHandler" "subprotocol\nHandler"

cookieToHandler :: Dot L.Text
cookieToHandler = transition "cookieToHandler" "cookieTo\nHandler"

cookie :: Dot L.Text
cookie = transition "cookie" "cookie"

executor :: Dot L.Text
executor = state "executor" "executor"

topLevelImplGraph :: G.DotGraph L.Text
topLevelImplGraph = digraph (Str "topLevelImpl") $ do
    graphAttrs [RankDir FromLeft]

    userThread; sendGetId; mbThread; receive;
    epToConnectionInfo; connectionInfo; atomicLong; ourIdToMessageMap; theirIdToMessageMap; messageInfo;
    subprotocolHandlers; executor; subprotocolHandler;
    cookieToHandler; cookie;

    cluster (Num (Int 0)) $ do
        "mbThread"   --> "receive"
        "userThread" --> "sendGetId"
    cluster (Num (Int 1)) $ do
        edge "connectionInfo" "atomicLong"          [textLabel "getId"]
        edge "connectionInfo" "ourIdToMessageMap"   [textLabel "send"]
        edge "connectionInfo" "theirIdToMessageMap" [textLabel "receive"]


    "sendGetId"           --> "epToConnectionInfo"
    "receive"             --> "epToConnectionInfo"
    "epToConnectionInfo"  --> "connectionInfo"

    "ourIdToMessageMap"   --> "messageInfo"
    "theirIdToMessageMap" --> "messageInfo"

    "receive"             --> "subprotocolHandlers"
    "subprotocolHandlers" --> "executor"
    edge "subprotocolHandler" "messageInfo" [textLabel "receive/closeId"]

    "sendGetId"           --> "cookieToHandler"
    "cookieToHandler"     --> "executor"
    edge "subprotocolHandler" "cookie" [textLabel "receipt"]

    "executor"            --> "subprotocolHandler"


--------------------------------------------------
-- epToConnectionInfo

mbThread :: Dot L.Text
mbThread = startEndClosedState "mbThread" "mbThread"

userThread :: Dot L.Text
userThread = startEndClosedState "userThread" "user\nthread"

-------------------------

epToConnectionInfoRead :: Dot L.Text
epToConnectionInfoRead = state "epToConnectionInfoRead" "epTo\nConnInfo\nread"

epToConnectionInfoWrite :: Dot L.Text
epToConnectionInfoWrite = state "epToConnectionInfoWrite" "epTo\nConnInfo\nwrite"

epToConnectionInfoWriteDone :: Dot L.Text
epToConnectionInfoWriteDone = state "epToConnectionInfoWriteDone" "write\ndone"

epNull :: Dot L.Text
epNull = decision "epNull" "null"

epNotNull :: Dot L.Text
epNotNull = decision "epNotNull" "not null"

-------------------------

getId  :: Dot L.Text
getId  = startEndClosedState "getId" "getId"

tGetIdNullFalse :: Dot L.Text
tGetIdNullFalse = transition "tGetIdNullFalse" "tGetIdNullFalse"

tGetIdNullTrue :: Dot L.Text
tGetIdNullTrue = transition "tGetIdNullTrue" "tGetIdNullTrue"

getNextId :: Dot L.Text
getNextId = startEndClosedState "getNextId" "getId\ngetNextId\ndone"

createCIPending :: Dot L.Text
createCIPending = state "createCIPending" "create\nConnInfo\nPENDING"

tGetIdWrtDone :: Dot L.Text
tGetIdWrtDone = transition "tGetIdWrtDone" "tGetIdWrtDone"

-------------------------

connect :: Dot L.Text
connect = startEndClosedState "connect" "connect\nsendInit"

tCntNullFalse :: Dot L.Text
tCntNullFalse = transition "tCntNullFalse" "tCntNullFalse"

tCntNullTrue :: Dot L.Text
tCntNullTrue = transition "tCntNullTrue" "tCntNullTrue"

sendInit :: Dot L.Text
sendInit = startEndClosedState "sendInit" "connect\nsendInit\ndone"

createCIConnected :: Dot L.Text
createCIConnected = state "createCIConnected" "create\nConnInfo\nCONNECTED"

tCntWrtDone :: Dot L.Text
tCntWrtDone = transition "tCntWrtDone" "tCntWrtDone"

-------------------------

disconnect :: Dot L.Text
disconnect = startEndClosedState "disconnect" "disconnect"

tDcntWrtDone :: Dot L.Text
tDcntWrtDone = startEndClosedState "tDcntWrtDone" "disconnect\ndone"

-------------------------

receiveInit :: Dot L.Text
receiveInit = startEndClosedState "receiveInit" "receiveInit"

tRcvInitDone :: Dot L.Text
tRcvInitDone = startEndClosedState "tRcvInitDone" "receiveInit\ndone"

-------------------------

send :: Dot L.Text
send = startEndClosedState "send" "send"

sendDone :: Dot L.Text
sendDone = startEndClosedState "sendDone" "send\ndone"

-------------------------

receive :: Dot L.Text
receive = startEndClosedState "receive" "receive"

receiveDone :: Dot L.Text
receiveDone = startEndClosedState "receiveDone" "receive\ndone"

-------------------------

epToConnectionInfoGraph :: G.DotGraph L.Text
epToConnectionInfoGraph = digraph (Str "epToConnectionInfo") $ do
    graphAttrs [RankDir FromLeft]

    mbThread; userThread; epToConnectionInfoRead; epNull; epNotNull; epToConnectionInfoWrite; epToConnectionInfoWriteDone;
    getId; tGetIdNullFalse; getNextId; tGetIdNullTrue; createCIPending; tGetIdWrtDone;
    connect; tCntNullFalse; tCntNullTrue; sendInit; createCIConnected; tCntWrtDone;
    disconnect; tDcntWrtDone;
    receiveInit; tRcvInitDone;
    send; sendDone;
    receive; receiveDone;

    cluster (Num (Int 0)) $ do
        "epToConnectionInfoRead"   --> "epNotNull"
        "epToConnectionInfoRead"   --> "epNull"
        "epToConnectionInfoWrite"  --> "epToConnectionInfoWriteDone"
    cluster (Num (Int 1)) $ do
        "mbThread" --> "connect"
        "mbThread" --> "disconnect"
        "mbThread" --> "receiveInit"
        "mbThread" --> "receive"
    cluster (Num (Int 2)) $ do
        "userThread" --> "getId"
        "userThread" --> "send"

    -- getID
    "getId"   --> "epToConnectionInfoRead"
    -- getID not null
    "epNotNull" --> "tGetIdNullFalse"
    "tGetIdNullFalse" --> "getNextId"
    -- getID null
    "epNull" --> "tGetIdNullTrue"
    "tGetIdNullTrue" --> "createCIPending"
    "createCIPending" --> "epToConnectionInfoWrite"
    "epToConnectionInfoWriteDone" --> "tGetIdWrtDone"
    "tGetIdWrtDone" --> "getNextId"

    -- connect
    "connect"   --> "epToConnectionInfoRead"
    -- connect not null
    "epNotNull" --> "tCntNullFalse"
    "tCntNullFalse" --> "sendInit"
    -- connect null
    "epNull" --> "tCntNullTrue"
    "tCntNullTrue" --> "createCIConnected"
    "createCIConnected" --> "epToConnectionInfoWrite"
    "epToConnectionInfoWriteDone" --> "tCntWrtDone"
    "tCntWrtDone" --> "sendInit"

    -- disconnect
    "disconnect" --> "epToConnectionInfoWrite"
    "epToConnectionInfoWriteDone" --> "tDcntWrtDone"

    -- receiveInit
    "receiveInit" --> "epToConnectionInfoRead"
    "epNotNull" --> "tRcvInitDone"

    -- send
    "send" --> "epToConnectionInfoRead"
    "epNotNull" --> "sendDone"

    -- receiveInit
    "receive" --> "epToConnectionInfoRead"
    "epNotNull" --> "receiveDone"

------------------------------------------------------------------------------
-- GRACEFUL_SHUTDOWN

gsdSent :: Dot L.Text
gsdSent = state "gsdSent" "GSD\nSENT"
gsdRcv :: Dot L.Text
gsdRcv = state "gsdRcv" "GSD\nRCV"
gsdAckSent :: Dot L.Text
gsdAckSent = state "gsdAckSent" "GSD\nACK\nSENT"
gsdAckRcv :: Dot L.Text
gsdAckRcv = state "gsdAckRcv" "GSD\nACK\nRCV"
qEq0I :: Dot L.Text
qEq0I = state "qEq0I" "Q==0"
qEq0R :: Dot L.Text
qEq0R = state "qEq0R" "Q==0"
disconnectI :: Dot L.Text
disconnectI = transition "disconnectI" "disconnect"
disconnectR :: Dot L.Text
disconnectR = transition "disconnectR" "disconnect"
adminRcvDone :: Dot L.Text
adminRcvDone = transition "adminRcvDone" "Admin\nRec\nDone"
subRcvDone :: Dot L.Text
subRcvDone = transition "subRcvDone" "Sub\nRec\nDone"

gsdImplState :: G.DotGraph L.Text
gsdImplState = digraph (Str "gsdImplState") $ do
    graphAttrs [RankDir FromLeft]
    initialized; gsdSent; gsdRcv; gsdAckSent; gsdAckRcv; qEq0I; qEq0R;
    disconnectI; disconnectR; adminRcvDone; subRcvDone;

    "Initialized" --> "gsdSent"
    "Initialized" --> "gsdRcv"

    "gsdSent"     --> "gsdAckRcv"
    "gsdAckRcv"   --> "qEq0I"
    "qEq0I"       --> "disconnectI"
    "disconnectI" --> "adminRcvDone"

    "gsdRcv"      --> "gsdAckSent"
    "gsdAckSent"  --> "qEq0R"
    "qEq0R"       --> "disconnectR"
    "disconnectR" --> "subRcvDone"

------------------------------------------------------------------------------
-- PROTOCOL DIAGRAMS

msgLifecycleHappyCaseGraph :: G.DotGraph L.Text
msgLifecycleHappyCaseGraph = digraph (Str "msgLifecycleHappyCase") $ do
    graphAttrs [RankDir FromLeft]
    closedStart; closedEnd; open; halfClosed;
    requestEndMessageP; requestEndMessageP2; requestOneWayMessageP; responseEndMessageP;
    open; openExpectDATA; halfClosed;
    startMessage; requestDATA; responseDATA;

    "ClosedStart"              -->  "startMessage"
    "startMessage"             -->  "Open"
    "Open"                     -->  "RequestEndMessageP"

    edge "RequestEndMessageP"       "OpenExpectDATA"        [textLabel "false"]
    "OpenExpectDATA"           -->  "requestDATA"
    "requestDATA"              -->  "RequestEndMessageP2"

    edge "RequestEndMessageP2"      "RequestOneWayMessageP" [textLabel "true"]

    edge "RequestEndMessageP2"      "OpenExpectDATA"        [textLabel "false"]

    edge "RequestEndMessageP"       "RequestOneWayMessageP" [textLabel "true"]

    edge "RequestOneWayMessageP"    "ClosedEnd"             [textLabel "true"]

    edge "RequestOneWayMessageP"    "HalfClosed"            [textLabel "false"]
    "HalfClosed"               -->  "responseDATA";
    "responseDATA"             -->  "ResponseEndMessageP"

    edge "ResponseEndMessageP"      "HalfClosed"            [textLabel "false"]
    edge "ResponseEndMessageP"      "ClosedEnd"             [textLabel "true"]

connectionInitializationLifecycleGraph :: G.DotGraph L.Text
connectionInitializationLifecycleGraph = digraph (Str "connectionInitializationLifecycle") $ do
    graphAttrs [RankDir FromLeft]
    connected; nonInitFrame; receiptOrTimeout; releaseConnection; released;
    initOne; initialized;
    initAfterInit;

    "Connected"         --> "nonInitFrame"
    "nonInitFrame"      --> "releaseConnection"
    "releaseConnection" --> "Released"

    "Connected"         --> "initOne"
    edge "initOne"          "Initialized"       [textLabel "version match"]
    edge "initOne"          "receiptOrTimeout" [textLabel "no common version"]
    "receiptOrTimeout"  --> "releaseConnection"

    "Initialized"       --> "initAfterInit"
    "initAfterInit"     --> "releaseConnection"

connectionGracefulShutdownLifecycleGraph :: G.DotGraph L.Text
connectionGracefulShutdownLifecycleGraph = digraph (Str "connectionGracefulShutdownLifecycle") $ do
    graphAttrs [RankDir FromLeft]
    initialized; releaseConnection; released;
    gracefulShutdown; graceFulSD; queuesDrained; receiptOrTimeout;
    gracefulShutdownAck; gracefulShutdownAckTimeout; graceFulSDAck;
    startMessageIdN; normalProcessing
    ignoreAnyFrameExceptDataForExisting;

    "Initialized"                --> "gracefulShutdown"
    "gracefulShutdown"           --> "GraceFulSD"
    "GraceFulSD"                 --> "gracefulShutdownAck"
    "gracefulShutdownAck"        --> "GraceFulSDAck"
    "GraceFulSD"                 --> "gracefulShutdownAckTimeout"
    "gracefulShutdownAckTimeout" --> "GraceFulSDAck"
    "GraceFulSDAck"              --> "queuesDrained"
    "queuesDrained"              --> "receiptOrTimeout"
    "receiptOrTimeout"           --> "releaseConnection"
    "releaseConnection"          --> "Released"

    "GraceFulSD"                 --> "startMessageIdN"
    "startMessageIdN"            --> "normalProcessing"

    "GraceFulSDAck"              --> "ignoreAnyFrameExceptDataForExisting"
    "ignoreAnyFrameExceptDataForExisting" --> "GraceFulSDAck"

msgLifecycleErrorsGraph :: G.DotGraph L.Text
msgLifecycleErrorsGraph = digraph (Str "msgLifecycleErrors") $ do
    graphAttrs [RankDir FromLeft]
    closedStart;
    openConnection;
    unknownFrameType; rstUnknownFrameType;
    startId0; rstStartId0;
    dataId0; rstDataId0;
    dataOnClosed; rstDataOnClosed;
    rstOnClosed;
    open; halfClosed; rstOpenOrHalfClosed;
    closedEnd;
    startOnNonClosed; rstStartOnNonClosed;

    "Open"                    --> "rstOpenOrHalfClosed"
    "HalfClosed"              --> "rstOpenOrHalfClosed"
    "rstOpenOrHalfClosed"     --> "ClosedEnd"

    "Open"                    --> "startOnNonClosed"
    "HalfClosed"              --> "startOnNonClosed"
    "startOnNonClosed"        --> "rstStartOnNonClosed"
    "rstStartOnNonClosed"     --> "ClosedEnd"

    "ClosedStart"             --> "dataOnClosed"
    "dataOnClosed"            --> "rstDataOnClosed"
    "rstDataOnClosed"         --> "ClosedStart"

    "ClosedStart"             --> "rstOnClosed"
    "rstOnClosed"             --> "ClosedStart"

    "OpenConnection"          --> "unknownFrameType"
    "unknownFrameType"        --> "rstUnknownFrameType"
    "rstUnknownFrameType"     --> "OpenConnection"

    "OpenConnection"          --> "startId0"
    "startId0"                --> "rstStartId0"
    "rstStartId0"             --> "OpenConnection"

    "OpenConnection"          --> "dataId0"
    "dataId0"                 --> "rstDataId0"
    "rstDataId0"              --> "OpenConnection"

------------------------------------------------------------------------------

cases :: [(FilePath, G.DotGraph L.Text)]
cases = [ -- PROTOCOL DIAGRAMS
          ("msgLifecycleHappyCase"               , msgLifecycleHappyCaseGraph)
        , ("connectionInitializationLifecycle"   , connectionInitializationLifecycleGraph)
        , ("connectionGracefulShutdownLifecycle" , connectionGracefulShutdownLifecycleGraph)
        , ("msgLifecycleErrors"                  , msgLifecycleErrorsGraph)
          -- IMPL DIAGRAMS
        , ("topLevelImpl"                        , topLevelImplGraph)
        , ("epToConnectionInfo"                  , epToConnectionInfoGraph)
        , ("gsdImplState"                        , gsdImplState)
        ]

main :: IO ()
main = doDots "/tmp" cases

-- End of file.
