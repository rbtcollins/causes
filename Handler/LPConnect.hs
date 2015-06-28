module Handler.LPConnect where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import qualified Web.Authenticate.OAuth as OA

import LaunchpadClient.Auth (lpoauth, removeToken, safeShowToken, tempOrAccessToken)
import LaunchpadClient.Token (Token(Access, Temporary))

type ConnectForm = ()

getLPConnectR :: Handler Html
getLPConnectR = do
    (formWidget, formEnctype) <- generateFormPost connectForm
    let submission = Nothing :: Maybe ConnectForm
        temp_token = Nothing :: Maybe OA.Credential
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Connect to LP"
        $(widgetFile "lpconnect")

postLPConnectR :: Handler Html
postLPConnectR = do
    ((result, formWidget), formEnctype) <- runFormPost connectForm
    action <- lookupPostParam "action"
    case (result, action) of
        (FormSuccess res, Just "connect") -> do
            _token <- runDB tempOrAccessToken
            let submission = res
                temp_token = case _token of
                  Temporary temp_creds -> Just temp_creds
                  Access _ -> Nothing :: Maybe OA.Credential
            defaultLayout $ do
                aDomId <- newIdent
                setTitle "Connect to LP"
                $(widgetFile "lpconnect")
        (FormSuccess res, Just "cancel") -> do
            let submission = res
                temp_token = Nothing
            _ <- runDB removeToken
            defaultLayout $ do
                aDomId <- newIdent
                setTitle "Connect to LP"
                $(widgetFile "lpconnect")
        _ -> do
            let submission = Nothing
                temp_token = Nothing
            defaultLayout $ do
                aDomId <- newIdent
                setTitle "Connect to LP"
                $(widgetFile "lpconnect")


connectForm :: Form ConnectForm
connectForm = renderBootstrap3 BootstrapBasicForm $ pure ()
