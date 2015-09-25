module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.Message   (AuthMessage(..))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Yesod.Auth.Owl       (YesodAuthOwl(..)
                            , authOwl'
                            , loginR
                            , setPassR
                            )
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3
import Yesod.Goodies.PNotify

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

-- Set up i18n messages. See the message folder.
-- Automatically generate AppMesage data type and data constructor Msg-prefixed.
-- and make it instance for RenderMessage class.
-- So, we can use renderMessage for AppMessage data.
mkMessage "App" "messages" "en"

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            pnotify master
            addScriptEither $ urlBootstrap3Js master
            addStylesheetEither $ urlBootstrap3Css master
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized HomeR _ = loggedInAuth
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

loggedInAuth :: Handler AuthResult
loggedInAuth = fmap (maybe AuthenticationRequired $ const Authorized) maybeAuthId

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

data Account = Account { ident :: Text
                       , password :: Text
                       } deriving (Show, Read, Eq)

accountForm :: (MonadHandler m, RenderMessage (HandlerSite m) msg,
                RenderMessage (HandlerSite m) FormMessage) =>
               (AppMessage -> msg) -> AForm m Account
accountForm render = Account
                     <$> areq textField bfs'account Nothing
                     <*> areq passwordField bfs'passwd Nothing
                     <*  bootstrapSubmit bs'submit
  where
    bfs'account = (bfs $ render MsgAccountID) { fsName = Just "ident" }
    bfs'passwd = (bfs $ render MsgPassword) { fsName = Just "password" }
    bs'submit =  BootstrapSubmit (render MsgLogin) "btn-primary" []

data ChangePassword = ChangePassword { current_pass :: Text
                                     , new_pass :: Text
                                     , new_pass2 :: Text
                                     } deriving (Show, Read, Eq)
changePasswordForm :: (MonadHandler m, RenderMessage (HandlerSite m) msg,
                       RenderMessage (HandlerSite m) FormMessage) =>
                      (AppMessage -> msg) -> AForm m ChangePassword
changePasswordForm render = ChangePassword
                            <$> areq passwordField bfs'cur Nothing
                            <*> areq passwordField bfs'new Nothing
                            <*> areq passwordField bfs'new2 Nothing
                            <*  bootstrapSubmit bs'submit
  where
    bfs'cur = (bfs $ render MsgCurrentPassword) { fsName = Just "current_pass" }
    bfs'new = (bfs $ render MsgNewPassword) { fsName = Just "new_pass" }
    bfs'new2 = (bfs $ render MsgNewPassword2) { fsName = Just "new_pass2" }
    bs'submit =  BootstrapSubmit (render MsgChangePassword) "btn-primary" []

hGrid :: BootstrapFormLayout
hGrid = BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = runDB $ do
      x <- getBy $ UniqueUser $ credsIdent creds
      case x of
        Just (Entity uid _) -> do
          return $ Authenticated uid
        Nothing -> return $ UserError InvalidLogin

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authOwl' defaultPNotify { _styling = Just BrightTheme }]

    authHttpManager = getHttpManager

    loginHandler = lift $ do
      y <- getYesod
      defaultLayout $ do
        setTitle "Login"
        mkLoginWidget y AuthR

instance YesodAuthOwl App where
  getOwlIdent = lift $ fmap (userIdent . entityVal) requireAuth
  clientId _ = mockingbird_clientId
  owlPubkey _ = owl_pub
  myPrivkey _ = mockingbird_priv
  endpoint_auth _ = owl_auth_service_url
  endpoint_pass _ = owl_pass_service_url
  mkLoginWidget _ toParent = do
    render <- getMessageRender
    (widget, enc) <- generateFormPost $ renderBootstrap3 hGrid $ accountForm render
    $(widgetFile "login")
  mkChangePasswordWidget _ toParent = do
    render <- getMessageRender
    (widget, enc) <- generateFormPost $ renderBootstrap3 hGrid $ changePasswordForm render
    $(widgetFile "change-password")

instance YesodJquery App where
  urlJqueryJs _ = Right "//ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js"
  urlJqueryUiJs _ = Right "//ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/jquery-ui.min.js"
  urlJqueryUiCss _ = Right "//ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/themes/trontastic/jquery-ui.css"

instance YesodJqueryPnotify App where
  urlJqueryUiCss _ = Right "//ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/themes/trontastic/jquery-ui.css"

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
