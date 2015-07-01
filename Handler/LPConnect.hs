module Handler.LPConnect where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import qualified Web.Authenticate.OAuth as OA

import LaunchpadClient.Auth (lpoauth, removeToken, safeShowToken, tempOrAccessToken, tempToAccessToken)
import LaunchpadClient.Token (Token(Access, Temporary))

-- TODO: figure out how to find out 'this pages URI'
lpwebauth :: OA.OAuth
lpwebauth = lpoauth { OA.oauthCallback = Just "http://localhost:3000/connect" }

type ConnectForm = ()

getLPConnectR :: Handler Html
getLPConnectR = do
    (formWidget, formEnctype) <- generateFormPost connectForm
    let submission = Nothing :: Maybe ConnectForm
        token = Nothing :: Maybe Token
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
            (new_token, _token) <- runDB $ tempOrAccessToken lpwebauth
            token' <- case new_token of
              False -> case _token of
                Access existing_token -> return $ Access existing_token
                Temporary temp_creds -> do
                  either_token <- runDB $ tempToAccessToken lpwebauth temp_creds
                  case either_token of
                    Left error_response -> return $ Temporary temp_creds
                    Right access_token -> return $ Access access_token
              True -> case _token of
                Temporary temp_creds -> return $ Temporary temp_creds
                Access _ -> error "inconcievable"
            let submission = res
                token = Just token'
            defaultLayout $ do
                aDomId <- newIdent
                setTitle "Connect to LP"
                $(widgetFile "lpconnect")
        (FormSuccess res, Just "cancel") -> do
            let submission = res
                token = Nothing :: Maybe Token
            _ <- runDB removeToken
            defaultLayout $ do
                aDomId <- newIdent
                setTitle "Connect to LP"
                $(widgetFile "lpconnect")
        _ -> do
            let submission = Nothing
                token = Nothing :: Maybe Token
            defaultLayout $ do
                aDomId <- newIdent
                setTitle "Connect to LP"
                $(widgetFile "lpconnect")


connectForm :: Form ConnectForm
connectForm = renderBootstrap3 BootstrapBasicForm $ pure ()
