import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import System.Directory
import Data.Yaml
import Data.Either

main :: IO ()
main = do
    -- get our config using decodeFileThrow
    eApp <- decodeFileEither "settings.yml" :: IO (Either ParseException App)
    case eApp of
      Left pe -> print "Could not parse yml file"
      Right app -> warp (port app) app

    
