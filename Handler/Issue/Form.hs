module Handler.Issue.Form where

import Prelude (read)

import Import as Import hiding ((\\))
import Yesod.Form.Bootstrap3
import Data.List ((\\))
import Data.Time.LocalTime

import Model.Fields


issueForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
             UserId -> (AppMessage -> Text) -> Maybe Issue -> AForm m Issue
issueForm uid render mv
  = Issue
    <$> areq textField (bfs'focus $ render MsgIssueSubject) (issueSubject <$> mv)
    <*> aopt textareaField (bfs' $ render MsgIssueDescription) (issueDescription <$> mv)
    <*> aopt dayField (bfs' $ render MsgIssueLimitDay) (issueLimitdate <$> mv)
    <*> aopt timeFieldTypeTime (bfs' $ render MsgIssueLimitTime) (issueLimittime <$> mv)
    <*> pure uid
    <*> lift (liftIO getCurrentTime)
    <*> lift (liftIO getCurrentTime)

hiddenIssueForm uid mv
  = Issue
    <$> areq hiddenField "" (issueSubject <$> mv)
    <*> aopt hiddenField "" (issueDescription <$> mv)
    <*> aopt hiddenField "" (issueLimitdate <$> mv)
    <*> aopt hiddenField "" (issueLimittime <$> mv)
    <*> pure uid
    <*> lift (liftIO getCurrentTime)
    <*> lift (liftIO getCurrentTime)

data Search = Search { users :: Maybe [Entity User] }

searchAndHiddenIssueForm :: UserId -> (AppMessage -> Text) -> Maybe Issue -> Maybe Search
                         -> AForm (HandlerT App IO) (Issue, Search)
searchAndHiddenIssueForm uid render mi ms
  = (,)
    <$> hiddenIssueForm uid mi
    <*> searchForm render ms

searchForm :: (AppMessage -> Text) -> Maybe Search -> AForm (HandlerT App IO) Search
searchForm render mv = Search <$> aopt (checkboxesField collect) (bfs' $ render MsgUsers) (users <$> mv)
  where
    collect :: Handler (OptionList (Entity User))
    collect = do
      entities <- runDB $ selectList [] [Asc UserIdent]
      optionsPersist [] [Asc UserIdent] userNameId
