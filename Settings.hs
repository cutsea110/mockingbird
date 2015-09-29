-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Control.Exception           (throw)
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

import Yesod.Auth.Owl              (ClientID
                                   ,PublicKey(..)
                                   ,PrivateKey(..)
                                   )

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Text
    -- ^ Base for all generated URLs.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .: "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appCopyright              <- o .: "copyright"
        appAnalytics              <- o .:? "analytics"

        return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

-- | TimeZone. this value used as `hoursToTimeZone tz'.
tz :: Int
tz = 9

-- |
-- Owl service URL
--
owl_auth_service_url :: String
owl_auth_service_url = "http://localhost:3002/srv/auth"
owl_pass_service_url :: String
owl_pass_service_url = "http://localhost:3002/srv/change-pass"

-- |
-- mockingbird RSA keys
--
mockingbird_clientId :: ClientID
mockingbird_clientId = "mockingbird"

mockingbird_pub :: PublicKey
mockingbird_pub = PublicKey { public_size = 256
                            , public_n = 26153640976640153519860660637352857750655721601394047799319251443924890697009910596857973411812052773686351503508482023681802723612596968513965694251298791722517803479608180853753105854856994427725446796233960468933058580744447480303580978441065805131828899609760843116845158251326329062696985949365791463692417427698327866165282682255409895945192576497582175675995717643612234818670105549786870908886602736653665906590907665163680800116842584595552715618816691495034564724657442481103819778035747690174403258896839484750632496630885051259937325380159702649492526451358677724476064075276076736567070784816761640813359
                            , public_e = 65537
                            }

mockingbird_priv :: PrivateKey
mockingbird_priv = PrivateKey { private_pub =
                                   PublicKey { public_size = 256
                                             , public_n = 26153640976640153519860660637352857750655721601394047799319251443924890697009910596857973411812052773686351503508482023681802723612596968513965694251298791722517803479608180853753105854856994427725446796233960468933058580744447480303580978441065805131828899609760843116845158251326329062696985949365791463692417427698327866165282682255409895945192576497582175675995717643612234818670105549786870908886602736653665906590907665163680800116842584595552715618816691495034564724657442481103819778035747690174403258896839484750632496630885051259937325380159702649492526451358677724476064075276076736567070784816761640813359
                                             , public_e = 65537
                                             }
                              , private_d = 5901798928903233752006031862395462307924492527320697818089512940845708661032387320398134928167118550895638378097669112840535582640442750924714873312517476094180627670780252172759733928734609313695036890146086048721348297154731430889568626732455287732038659022060105113981766717714650968280905827321523566177004987875850018896462472714613348531057906875923467221044219788269499768966530884195354310628447212689367085723189881076121304012298532887866318488728918886046233291280077760886181888263142194213814042764572371565382800712897497429192567243059919485494363372382831823983651269927299324494045122869986125452825
                              , private_p = 166546499652984118961442043337452096748679202886611619699277873182296471783051115424434830391879698710486557691775792134193141111519688185287196775479617728267022525378108477144529667037123107566233022056623371071177467770639865305496972885951923016638380688140603488671620312789057742606720423060863188622317
                              , private_q = 157035068471170610966512083218902521786108199598108429440328318417077006785659170483138258787065658943716863468021234305722570016614467857145300113481968145372451847624559765322620801978302046791635588070064006351001948170464721486457846617649665945597124170422506634991910577198938945994549075831881905852427
                              , private_dP = 0
                              , private_dQ = 0
                              , private_qinv = 0
                              }

-- |
-- Owl RSA keys
--
owl_pub :: PublicKey
owl_pub = PublicKey { public_size = 256
                    , public_n = 23935502722801508291122222398018117881284958223263854065673689606867055652122077115632498984650750679970467900697728966520426415008444072251453446123881488809248692462117519335720631061157343736650249371835293662619945999329307142886808914215692490190245599500864907497806854772652186075160282343362861100625964817657470875052275949634580109631117392627776939182328215081842240646543745078419135398375800047086393491931547537516953037019818981085723402984601825491050312705896863144307436654552505557222743591857763940190952404403348742192979262305085887506928609325609473826220183742944601830381993567783603917096371
                    , public_e = 65537
                    }
