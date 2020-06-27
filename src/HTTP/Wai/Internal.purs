module Network.Wai.Internal where

import Prelude

import Data.List.Lazy (List, intercalate)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Network.HTTP.Types as H
import Node.Buffer (Buffer)
import Node.Stream (Readable)

data Request = Request {
  -- | Request method such as GET.
     method        :: H.Method
  -- | HTTP version such as 1.1.
  ,  httpVersion          :: H.HttpVersion
  -- | Extra path information sent by the client. The meaning varies slightly
  -- depending on backend; in a standalone server setting, this is most likely
  -- all information after the domain name. In a CGI application, this would be
  -- the information following the path to the CGI executable itself.
  --
  -- Middlewares and routing tools should not modify this raw value, as it may
  -- be used for such things as creating redirect destinations by applications.
  -- Instead, if you are writing a middleware or routing framework, modify the
  -- @pathInfo@ instead. This is the approach taken by systems like Yesod
  -- subsites.
  --
  -- /Note/: At the time of writing this documentation, there is at least one
  -- system (@Network.Wai.UrlMap@ from @wai-extra@) that does not follow the
  -- above recommendation. Therefore, it is recommended that you test the
  -- behavior of your application when using @rawPathInfo@ and any form of
  -- library that might modify the @Request@.
  ,  rawPathInfo          :: String
  -- | If no query string was specified, this should be empty. This value
  -- /will/ include the leading question mark.
  -- Do not modify this raw value - modify queryString instead.
  ,  rawQueryString       :: String
  -- | A list of headers (a pair of key and value) in an HTTP request.
  ,  requestHeaders       :: H.RequestHeaders
  -- | The client\'s host information.
  ,  remoteHost           :: String
  -- | Path info in individual pieces - the URL without a hostname/port and
  -- without a query string, split on forward slashes.
  ,  pathInfo             :: List String
  -- | Parsed query string information.
  ,  queryString          :: H.Query
  ,  body          :: Readable ()
  -- | The size of the request body. In the case of a chunked request body,
  -- this may be unknown.
  ,  bodyLength     :: RequestBodyLength
  -- | The value of the Host header in a HTTP request.
  ,  headerHost     :: Maybe String
  -- | The value of the Range header in a HTTP request.
  ,  headerRange   :: Maybe String
  -- | The value of the Referer header in a HTTP request.ce 3.2.0
  ,  headerReferer   :: Maybe String
  -- | The value of the User-Agent header in a HTTP request.
  ,  headerUserAgent :: Maybe String
  }


instance showRequest :: Show Request where
    show (Request req) = "Request {" <> intercalate ", " str <> "}"
        where   
            str = do
                (Tuple a b) <- fields
                pure (a <> " = " <> b)
            fields =
                [ (Tuple "requestMethod" (show req.method))
                , (Tuple "httpVersion" (show req.httpVersion))
                , (Tuple "rawPathInfo" $ show req.rawPathInfo)
                , (Tuple "rawQueryString" $ show req.rawQueryString)
                , (Tuple "requestHeaders" $ show req.requestHeaders)
                , (Tuple "remoteHost" $ show req.remoteHost)
                , (Tuple "pathInfo" $ show req.pathInfo)
                , (Tuple "queryString" $ show req.queryString)
                , (Tuple "requestBody" "<Readable>")
                , (Tuple "requestBodyLength" $ show req.bodyLength)
                , (Tuple "requestHeaderHost" $ show req.headerHost)
                , (Tuple "requestHeaderRange" $ show req.headerRange)
                ]


data Response
    = ResponseFile H.Status H.ResponseHeaders FilePath
    | ResponseString H.Status H.ResponseHeaders String
    | ResponseStream H.Status H.ResponseHeaders (Readable ())
    | ResponseRaw (Aff Buffer -> (Buffer -> Aff Unit) -> Aff Unit) Response

-- | The size of the request body. In the case of chunked bodies, the size will
-- not be known.
data RequestBodyLength = ChunkedBody | KnownLength Int

instance showRequestBodyLenght :: Show RequestBodyLength where
    show ChunkedBody = "<ChunkedBody>"
    show (KnownLength n) =  "KnownLength " <> show n

-- | A special datatype to indicate that the WAI handler has received the
-- response. This is to avoid the need for Rank2Types in the definition of
-- Application.
--
-- It is /highly/ advised that only WAI handlers import and use the data
-- constructor for this data type.
data ResponseReceived = ResponseReceived

type FilePath = String