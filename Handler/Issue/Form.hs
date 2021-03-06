module Handler.Issue.Form where

import Import as Import

import qualified Data.Text as T
import Yesod.Form.Bootstrap3
import Model.Fields

selfIssueForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
                 UserId -> (AppMessage -> Text) -> Maybe Issue -> AForm m Issue
selfIssueForm uid render mv
  = Issue
    <$> areq textField bfs'subj (issueSubject <$> mv)
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> pure uid
    <*> pure PUBLIC
    <*> lift (liftIO getCurrentTime)
    <*> lift (liftIO getCurrentTime)
  where
    bfs'subj = bfs'focus (render MsgIssueSubject) (render MsgCreateTaskForYourself)

issueForm :: RenderMessage site FormMessage =>
             UserId -> (AppMessage -> Text) -> Maybe Issue -> AForm (HandlerT site IO) Issue
issueForm uid render mv
  = Issue
    <$> areq textField bfs'subj (issueSubject <$> mv)
    <*> aopt textareaField bfs'desc (issueDescription <$> mv)
    <*> aopt dayField bfs'day (issueLimitdate <$> mv)
    <*> aopt timeFieldTypeTime bfs'time (issueLimittime <$> mv)
    <*> pure uid
    <*> areq (selectFieldList scopes) bfs'scope (issueScope <$> mv)
    <*> lift (liftIO getCurrentTime)
    <*> lift (liftIO getCurrentTime)
    where
      scopes :: [(Text, Scope)]
      scopes = [(render MsgPUBLIC, PUBLIC), (render MsgPRIVATE, PRIVATE)]
      bfs'subj = bfs'focus (render MsgIssueSubject) (render MsgSimpleAndClarity)
      bfs'desc = bfs' (render MsgIssueDescription) (render MsgInDetail)
      bfs'day = bfs'  (render MsgIssueLimitDay) (render MsgIssueLimitDay)
      bfs'time = bfs'  (render MsgIssueLimitTime) (render MsgIssueLimitTime)
      bfs'scope = bfs' (render MsgIssueScope) (render MsgIssueScope)

hiddenIssueForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
                   UserId -> Maybe Issue -> AForm m Issue
hiddenIssueForm uid mv
  = Issue
    <$> areq hiddenField "" (issueSubject <$> mv)
    <*> aopt hiddenField "" (issueDescription <$> mv)
    <*> aopt hiddenField "" (issueLimitdate <$> mv)
    <*> aopt hiddenField "" (issueLimittime <$> mv)
    <*> pure uid
    <*> areq hiddenField "" (issueScope <$> mv)
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
searchForm render mv = Search <$> aopt (usersFields collect) bfs'users (users <$> mv)
  where
    collect :: Handler (OptionList (Entity User))
    collect = optionsPersist [] [Asc UserIdent] userNameId
    bfs'users = bfs' (render MsgUsers) (render MsgUsers)
