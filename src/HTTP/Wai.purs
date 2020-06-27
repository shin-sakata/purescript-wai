module Network.Wai
( module Network.Wai.Internal
, Application
, Middleware
, responseFile
, responseString
, responseStream
, responseRaw
) where

import Prelude (Unit)

import Network.Wai.Internal
import Effect.Aff (Aff)
import Network.HTTP.Types as H
import Node.Buffer (Buffer)
import Node.Stream (Readable)

type Application = Request -> (Response -> Aff ResponseReceived) -> Aff ResponseReceived

type Middleware = Application -> Application

responseFile :: H.Status -> H.ResponseHeaders -> FilePath -> Response
responseFile = ResponseFile

responseString :: H.Status -> H.ResponseHeaders -> String -> Response
responseString = ResponseString


responseStream :: H.Status -> H.ResponseHeaders -> (Readable ()) -> Response
responseStream = ResponseStream

responseRaw :: (Aff Buffer -> (Buffer -> Aff Unit) -> Aff Unit) -> Response -> Response
responseRaw = ResponseRaw