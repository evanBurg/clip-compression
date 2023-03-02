import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import System.Directory
import Data.Yaml
import Data.Either
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import Network.Wai

main :: IO ()
main = do
    eApp <- decodeFileEither "settings.yml" :: IO (Either ParseException App)
    case eApp of
      Left pe -> print "Could not parse yml file"
      Right app -> do
        plainApp <- toWaiApp app
        run (port app) $ simpleCors plainApp

    
