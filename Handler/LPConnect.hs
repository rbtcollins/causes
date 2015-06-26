module Handler.LPConnect where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import LaunchpadClient.Auth (tempOrAccessToken)

type ConnectForm = ()

getLPConnectR :: Handler Html
getLPConnectR = do
    (formWidget, formEnctype) <- generateFormPost connectForm
    let submission = Nothing :: Maybe ConnectForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Connect to LP"
        $(widgetFile "lpconnect")

postLPConnectR :: Handler Html
postLPConnectR = do
    ((result, formWidget), formEnctype) <- runFormPost connectForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    token <- runDB tempOrAccessToken

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Connect to LP"
        $(widgetFile "lpconnect")

connectForm :: Form ConnectForm
connectForm = renderBootstrap3 BootstrapBasicForm $ pure ()
