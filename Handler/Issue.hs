module Handler.Issue where

import Prelude (read)

import Import as Import
import Yesod.Form.Bootstrap3
import Yesod.Goodies.PNotify

import Model.Fields

runForm = runFormPost . renderBootstrap3 Import.hGrid
genForm = generateFormPost . renderBootstrap3 Import.hGrid
bfs' = withPlaceholder <*> bfs
bfs'focus = withAutofocus <$> bfs'

issueForm
  :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
     Key User -> (AppMessage -> Text) -> Maybe Issue -> AForm m Issue
issueForm uid render mv = Issue
                          <$> areq textField (bfs'focus $ render MsgIssueSubject) (issueSubject <$> mv)
                          <*> aopt textareaField (bfs' $ render MsgIssueDescription) (issueDescription <$> mv)
                          <*> aopt dayField (bfs' $ render MsgIssueLimitDay) (issueLimitdate <$> mv)
                          <*> aopt timeFieldTypeTime (bfs' $ render MsgIssueLimitTime) (issueLimittime <$> mv)
                          <*> pure uid
                          <*> lift (liftIO getCurrentTime)
                          <*> lift (liftIO getCurrentTime)
                          <*  bootstrapSubmit (BootstrapSubmit (render MsgCreateIssue) "btn-primary" [])

simpleForm
  :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
     Key User -> (AppMessage -> Text) -> Maybe Issue -> AForm m Issue
simpleForm uid render mv = Issue
                          <$> areq textField (bfs'focus $ render MsgIssueSubject) (issueSubject <$> mv)
                          <*> pure Nothing
                          <*> pure Nothing
                          <*> pure Nothing
                          <*> pure uid
                          <*> lift (liftIO getCurrentTime)
                          <*> lift (liftIO getCurrentTime)
                          <*  bootstrapSubmit (BootstrapSubmit (render MsgCreateIssue) "btn-primary" [])

getNewIssueR :: Handler Html
getNewIssueR = do
  render <- getMessageRender
  uid <- requireAuthId
  mode <- lookupGetParam "mode"
  case mode of
    Just "simple" -> simpleView render uid
    Just _ -> defaultView render uid
    Nothing -> defaultView render uid
  where
    -- default
    defaultView render uid = do
      (w, enc) <- genForm $ issueForm uid render Nothing
      defaultLayout $ do
        setTitleI MsgCreateNewIssue
        $(widgetFile "new-issue")
    -- simple
    simpleView render uid = do
      (w, enc) <- genForm $ simpleForm uid render Nothing
      defaultLayout $ do
        setTitleI MsgCreateNewIssue
        $(widgetFile "new-issue-simple")
      
postNewIssueR :: Handler ()
postNewIssueR = do
  render <- getMessageRender
  uid <- requireAuthId
  ((r, _), _) <- runForm $ issueForm uid render Nothing
  case r of
    FormSuccess issue -> do
      iid <- runDB $ insert issue
      setPNotify $ defNotify { _type = Just Success
                             , _title = Just $ Right $ render MsgSuccess
                             , _text = Just $ Right $ render MsgSucceedToCreateIssue
                             }
      redirect (ISSUE $ IssueR iid)
    FormFailure (x:_) -> do
      setPNotify $ defNotify { _type = Just Error
                             , _title = Just $ Right $ render MsgError
                             , _text = Just $ Right x
                             }
    _ -> do
      setPNotify $ defNotify { _type = Just Error
                             , _title = Just $ Right $ render MsgError
                             , _text = Just $ Right $ render MsgFailureToCreateIssue
                             }
  redirect (ISSUE NewIssueR)

getIssueR :: IssueId -> Handler Html
getIssueR key = do
  (issue, opener) <- runDB $ do
    issue <- get404 key
    opener <- get404 (issueOpener issue)
    return (issue, opener)
  now <- liftIO getCurrentTime
  let createdBefore = (issueCreated issue) `beforeFrom` now
  defaultLayout $ do
    setTitleI $ MsgSubject issue
    $(widgetFile "issue")

data Search = Search { query :: Text
                     , users :: Maybe [Text]
                     }

searchForm :: (AppMessage -> Text) -> Maybe Search -> AForm (HandlerT App IO) Search
searchForm render mv = Search
                       <$> areq (searchField True) (bfs' $ render MsgUserNameOrIdent) (query <$> mv)
                       <*  bootstrapSubmit (BootstrapSubmit (render MsgSearch) "btn-primary" [])
                       <*> aopt (checkboxesField collect) (bfs' $ render MsgUsers) (users <$> mv)

  where
    collect :: Handler (OptionList Text)
    collect = do
      entities <- runDB $ selectList [] [Asc UserIdent]
      let entities' = filter (match (query <$> mv) (users <$> mv)) entities
      return (mkopts entities'){ olReadExternal = Just }
    mkopts = mkOptionList . map ((Option <$> userName <*> userIdent <*> userIdent).entityVal)
    match Nothing _ _ = False
    match (Just q) mus (Entity uid u) =  q `isInfixOf` userIdent u ||
                                         q `isInfixOf` userName u -- ||
--                                         maybe False (uid `elem`) mus

getNewChannelR :: IssueId -> Handler Html
getNewChannelR key = do
  render <- getMessageRender
  Just logic <- lookupGetParam "logic"
  let uri = (ISSUE $ NewChannelR key, [("logic", logic)])
  issue <- runDB $ get404 key
  (w, enc) <- genForm $ searchForm render Nothing
  defaultLayout $ do
    setTitleI $ MsgSubject issue
    $(widgetFile "new-channel")

postNewChannelR :: IssueId -> Handler Html
postNewChannelR key = do
  render <- getMessageRender
  ml <- lookupPostParam "logic"
  let logic = maybe "ALL" id ml
      uri = (ISSUE $ NewChannelR key, [("logic", logic)])
  ((r, _), _) <- runForm $ searchForm render Nothing
  case r of
    FormSuccess s -> do
      issue <- runDB $ get404 key
      ((_, w), enc) <- runForm $ searchForm render (Just s)
      defaultLayout $ do
        setTitleI MsgCreateNewIssue
        $(widgetFile "new-channel")
    FormFailure (x:_) -> invalidArgs [x]
    _ -> invalidArgs ["error occured"]
