{-# LANGUAGE FlexibleContexts
           , GADTs
           , MultiParamTypeClasses
           , OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell
           , TypeFamilies
           , GeneralizedNewtypeDeriving
 #-}
import Yesod
import Yesod.Form.Bootstrap3
import Control.Monad.Logger (runNoLoggingT)
import Database.Persist
import Database.Persist.Sqlite
import Control.Applicative (pure, (<$>), (<*>))
import Data.Text (Text, concat)
import Data.List (intersperse)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Product
    name Text
    deriving Show
Category
    name Text
    deriving Show
ProductCategory
    product ProductId
    category CategoryId
    deriving Show
|]

data App = App ConnectionPool

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- load Twitter Bootstrap styles
addStyle :: Widget
addStyle = addStylesheetRemote "http://netdna.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"

getHomeR :: Handler Html
getHomeR = do
    rows <- productsAndCategories
    ((result, formWidget), enctype) <- runFormGet $ productForm Nothing

    defaultLayout $ do
        addStyle
        [whamlet|$newline never
<div .container>
    <div .row>
        <h2>
            Add new product
        <form method=post enctype=#{enctype}>
            ^{formWidget}
            <input type=submit .btn .btn-primary value="Save">
        <h2>
            Products
        <table .table>
            <tr>
                <th>
                    Product name
                <th>
                    Categories
            $forall row <- rows
                <tr>
                    <td>
                        #{productName $ fst row}
                    <td>
                        #{Data.Text.concat $ intersperse ", " (Prelude.map categoryName (snd row))}
|]

productsAndCategories :: HandlerT App IO [(Product, [Category])]
productsAndCategories = runDB $ selectList [] [Asc ProductName] >>= mapM (\(Entity kp p) -> do
    categoryProducts <- selectList [ProductCategoryProduct ==. kp] []
    let categoryIds = Prelude.map (productCategoryCategory . entityVal) categoryProducts
    categoryEntities <- selectList [CategoryId <-. categoryIds] []
    return (p, Prelude.map entityVal categoryEntities))                                       

postHomeR :: Handler Html
postHomeR = do
    ((result, _), _) <- runFormPostNoToken $ productForm Nothing
    case result of 
        FormSuccess (product, maybeCategories) -> do
            p <- runDB $ insert product
            case maybeCategories of
                Just c -> mapM_ (\c' -> runDB $ insert $ ProductCategory p c') c 
                Nothing -> return ()
            redirect HomeR
        _ -> do
            setMessage "Failure adding"
            redirect HomeR

productForm :: Maybe Product -> Html -> MForm (HandlerT App IO) (FormResult (Product, Maybe [CategoryId]), Widget)
productForm mproduct = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> product
    <*> mcategories
    where
        product = Product
            <$> areq textField "Name" (productName <$> mproduct)
        mcategories = aopt (checkboxesField categories) "Categories" Nothing
            where
                categories = do
                    entities <- runDB $ selectList [CategoryName !=. ""] [Asc CategoryName]
                    optionsPairs $ Prelude.map (\cat -> (categoryName $ entityVal cat, entityKey cat)) entities
                categories :: HandlerT App IO (OptionList CategoryId)

main :: IO ()
main = runNoLoggingT $ withSqlitePool "dev.db3" 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        -- add some example data

        -- categories
        home <- insert $ Category "Home, Garden & Tools"
        kitchen <- insert $ Category "Kitchen & Dining"
        toys <- insert $ Category "Toys & Games"
        clothing <- insert $ Category "Clothing"

        -- products
        chair <- insert $ Product "Vinyl chair"
        insert $ ProductCategory chair home

        coffeemaker <- insert $ Product "Coffeemaker"
        insert $ ProductCategory coffeemaker kitchen
        -- and the second category:
        insert $ ProductCategory coffeemaker home

        nerf <- insert $ Product "Nerf Blaster"
        insert $ ProductCategory nerf toys

        dress <- insert $ Product "Urban Sprawl Print Hi-low Dress"
        insert $ ProductCategory dress clothing

        insert $ Product "Milkshake"
        -- no category

        return ()

    warp 3005 $ App pool

