module ACL.Tracing where

import Monitor.Tracing.Zipkin (Zipkin)
import Monitor.Tracing.Zipkin qualified as ZPK
import Network.Socket (HostName)

newZipkin
  :: HostName
  -- ^ Zipkin server URL
  -> IO Zipkin
newZipkin serverURL = do
  let settings =
        ZPK.defaultSettings
          { ZPK.settingsEndpoint =
              Just $
                ZPK.defaultEndpoint
                  { ZPK.endpointService = Just "acl"
                  }
          , ZPK.settingsHostname = Just serverURL
          , ZPK.settingsPublishPeriod = 2
          }
  ZPK.new settings
