module Util.Widget ( wGravatar
                   , wGravatar'
                   , wGravatarTiny'

                   , wGravatarRoute
                   , wGravatarRoute'
                   , wGravatarRouteTiny'
                   )where

import Import
import Yesod.Core

import Text.Blaze (ToMarkup)

wGravatar :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => User -> WidgetT site m ()
wGravatar = wGravatarRoute (Right ("https://gravatar.com"::Text))

wGravatar' :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => User -> WidgetT site m ()
wGravatar' = wGravatarRoute' (Right ("https://gravatar.com"::Text)) True

wGravatarTiny' :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => User -> WidgetT site m ()
wGravatarTiny' = wGravatarRouteTiny' (Right ("https://gravatar.com"::Text))

wGravatarRoute :: (MonadIO m, MonadBaseControl IO m, ToMarkup a, MonadThrow m) =>
                  Either (Route site) a -> User -> WidgetT site m ()
wGravatarRoute r = wGravatarRoute' r False

wGravatarRoute' :: (MonadIO m, MonadBaseControl IO m, ToMarkup a, MonadThrow m) =>
                  Either (Route site) a -> Bool -> User -> WidgetT site m ()
wGravatarRoute' (Right r) b u =
  [whamlet|<a href="#{r}" class="media-left">
             <img class="hidden-xs" src="#{userGravatar u}">
             <img class="visible-xs" src="#{userGravatarSmall u}">
             $if b
               <span class="small text-muted">#{userName u}
          |]
wGravatarRoute' (Left r) b u =
  [whamlet|<a href="@{r}" class="media-left">
             <img class="hidden-xs" src="#{userGravatar u}">
             <img class="visible-xs" src="#{userGravatarSmall u}">
             <span class="small text-muted">#{userName u}
             $if b
               <span class="small text-muted">#{userName u}
          |]

wGravatarRouteTiny' :: (MonadIO m, MonadBaseControl IO m, ToMarkup a, MonadThrow m) =>
                       Either (Route site) a -> User -> WidgetT site m ()
wGravatarRouteTiny' (Right r) u =
  [whamlet|<a href="#{r}">
             <img class="gravatar" src="#{userGravatarTiny u}" alt=Gravatar>
             <span class="small text-muted">#{userName u}
          |]
wGravatarRouteTiny' (Left r) u =
  [whamlet|<a href="@{r}">
             <img class="gravatar" src="#{userGravatarTiny u}" alt=Gravatar>
             <span class="small text-muted">#{userName u}
          |]

