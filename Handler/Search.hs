module Handler.Search ( postSearchR
                      ) where

import Import hiding (Status)
import Data.Text as T (append)
import Database.Persist.Sql

import Model.Fields
import Handler.Common (toFullEquipedComments, toFullEquipedIssue)

postSearchR :: Handler Html
postSearchR = do
  uid <- requireAuthId
  now <- liftIO getCurrentTime
  Just q <- lookupPostParam "q"
  rs <- searcher uid q
  let createdBeforeC c = (commentCreated c) `beforeFrom` now
      createdBeforeI i = (issueCreated i) `beforeFrom` now
  defaultLayout $ do
    setTitleI $ MsgSearchResult q
    $(widgetFile "search-result")

searcher :: UserId -> Text -> Handler [Either FullEquipedIssue FullEquipedComment]
searcher uid q = do
  let q' = "%" `T.append` q `T.append` "%"
  runDB $ do
    is <- issues uid q
    is' <- toFullEquipedIssue is
    cs <- comments uid q
    cs' <- toFullEquipedComments cs
    return $ sortBy cmp $ map Left is' ++ map Right cs'
  where
    fst4 (x, _, _, _) = x
    snd6 (_, x, _, _, _, _) = x
    rev LT = GT
    rev GT = LT
    rev EQ = EQ
    compare' = (rev .) . compare
    cmp = compare' `on` either (issueUpdated . entityVal . fst4) (commentUpdated . entityVal . snd6)

    
issues :: MonadIO m => UserId -> Text -> ReaderT SqlBackend m [Entity Issue]
issues uid q = rawSql sql [toPersistValue PRIVATE, toPersistValue uid, toPersistValue uid, toPersistValue uid, toPersistValue q', toPersistValue q']
    where
      q' = "%" `T.append` q `T.append` "%"
      sql :: Sql
      sql = "SELECT \
                DISTINCT ?? \
             FROM \
                issue, \
                channel, \
                ticket \
             WHERE \
                   issue.id=channel.issue \
               AND issue.id NOT IN (SELECT id FROM issue WHERE scope=? AND opener<>?) \
               AND channel.id=ticket.channel \
               AND (ticket.domain=? OR ticket.codomain=?) \
               AND (issue.subject like ? OR issue.description like ?)"

comments :: MonadIO m => UserId -> Text -> ReaderT SqlBackend m [Entity Comment]
comments uid q = rawSql sql [toPersistValue PRIVATE, toPersistValue uid, toPersistValue uid, toPersistValue uid, toPersistValue q', toPersistValue q']
    where
      q' = "%" `T.append` q `T.append` "%"
      sql :: Sql
      sql = "WITH ts AS \
             (SELECT DISTINCT id \
              FROM ticket \
              WHERE channel IN (SELECT DISTINCT channel.id \
                                FROM channel, \
                                     ticket \
                                WHERE \
                                      channel.issue NOT IN (SELECT id FROM issue WHERE scope=? AND opener<>?) \
                                  AND channel.id=ticket.channel \
                                  AND (ticket.domain=? OR ticket.codomain=?))) \
             SELECT \
                 DISTINCT ?? \
             FROM \
                comment LEFT OUTER JOIN stored_file ON comment.id=stored_file.comment \
             WHERE \
                   (comment.comment LIKE ? OR stored_file.fullname LIKE ?) \
               AND comment.ticket IN (SELECT id FROM ts)"
