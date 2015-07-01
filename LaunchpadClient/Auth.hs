{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module LaunchpadClient.Auth (
    lpoauth,
    maybeToken,
    migrateAll,
    removeToken,
    safeShowToken,
    tempOrAccessToken,
    tempToAccessToken,
    ) where


-- import Data.ByteString.Char8 ()

-- simple
import Prelude
import Control.Concurrent
import Control.Exception (toException, throwIO)
-- import Control.Monad
import Debug.Trace
import qualified Data.Text as DT
-- import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans
import Control.Monad.Trans.Reader as Reader (ReaderT)
import Control.Monad.Trans.Control as Control (MonadBaseControl)
-- import Data.Maybe
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Exception (SomeException)
import Network.HTTP.Conduit
-- import System.Random (randomRIO)
-- qualified
-- import Data.Time.Calendar (Day(ModifiedJulianDay))
-- import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime(..))
-- import qualified Data.ByteString.Char8 as BS
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
-- import qualified Data.Text.Lazy          as TL
-- import qualified Data.Text.Lazy.Encoding as LE
import qualified Network.HTTP.Types as W
import qualified Web.Authenticate.OAuth as OA
-- local
import LaunchpadClient.Token
-- local qualified


lpoauth :: OA.OAuth
lpoauth = OA.newOAuth { OA.oauthServerName = "api.launchpad.net"
                      , OA.oauthConsumerKey = "haskell"
                      , OA.oauthConsumerSecret = ""
                      , OA.oauthRequestUri = "https://launchpad.net/+request-token"
                      , OA.oauthSignatureMethod = OA.PLAINTEXT
                      , OA.oauthAuthorizeUri = "https://launchpad.net/+authorize-token"
                      , OA.oauthAccessTokenUri  = "https://launchpad.net/+access-token"
                      }


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Server
    name String
    credentials Token
    UniqueServer name
|]

serverFile :: DT.Text
serverFile = "servers.sqllite"

safeShowCreds :: OA.Credential -> String
safeShowCreds creds = show $ filter ((`elem` ["oauth_token"]) . fst) $ OA.unCredential creds

safeShowToken :: Token -> String
safeShowToken (Temporary creds) = safeShowCreds creds
safeShowToken (Access creds) = safeShowCreds creds


tempOrAccessToken :: (Control.MonadBaseControl IO m, MonadIO m) =>
                     OA.OAuth ->
                     ReaderT SqlBackend m (Bool, Token)
tempOrAccessToken oa =
  do
        maybeServer <- getBy $ UniqueServer "Launchpad"
        case maybeServer of
            Nothing -> do
                temp_creds <- trace "requesting temporary credentials" (withManager $ \manager -> OA.getTemporaryCredential oa manager)
                -- TODO: have something report on pending tokens for users - e.g. in a web UI
                let msg = "New token requested, authorise at " ++ show (OA.authorizeUrl oa temp_creds)
                _ <- insert $ trace msg (Server "Launchpad" (Temporary temp_creds))
                return $ (True, Temporary temp_creds)
            Just (Entity _ (Server _ temp_creds)) -> trace ("using cached credentials " ++ safeShowToken temp_creds) $ return (False, temp_creds)

maybeToken :: IO (Maybe OA.Credential)
maybeToken =  do
  (new_token, a_token) <- liftIO $ runSqlite serverFile $ tempOrAccessToken lpoauth
  case a_token of
    -- Try for 30 seconds (interactive testing/watching logfiles - probably
    -- want to discard if in a real app and just drive the state machine)
    Temporary temp_creds -> do
        access_token <- withManager $ \manager -> untilAuthorized manager lpoauth temp_creds 30
        runSqlite serverFile $
          updateWhere [ServerName ==. "Launchpad"] [ServerCredentials =. Access access_token]
        return $ trace ("authorized token " ++ safeShowCreds access_token) (Just access_token)
    Access token -> return (Just token)

removeToken :: MonadIO m => ReaderT SqlBackend m ()
removeToken = do
  deleteWhere [ServerName ==. "Launchpad"]

maybeAccessToken :: MonadIO m
                 => Manager
                 -> OA.OAuth
                 -> OA.Credential
                 -> m (Either (Response BSL.ByteString) OA.Credential)
maybeAccessToken manager a_lpoauth temp_creds =
  OA.getAccessTokenWith $ (OA.defaultAccessTokenRequest a_lpoauth temp_creds manager) {
        OA.accessTokenAddAuth = OA.addAuthBody
        , OA.accessTokenRequestHook = \req -> req { checkStatus = checkFor401 }}

tempToAccessToken :: (MonadBaseControl IO m, MonadIO m)
                  => OA.OAuth
                  -> OA.Credential
                  -> ReaderT SqlBackend m (Either (Response BSL.ByteString) OA.Credential)
tempToAccessToken a_lpoauth temp_creds = do
        maybe_access_token <- withManager $ \manager -> maybeAccessToken manager a_lpoauth temp_creds
        case maybe_access_token of
          Left error_response -> return $ Left error_response
          Right access_token -> do
            updateWhere [ServerName ==. "Launchpad"] [ServerCredentials =. Access access_token]
            return $ trace ("authorized token " ++ safeShowCreds access_token) (Right access_token)


untilAuthorized :: (MonadIO m, Ord a, Num a)
                => Manager
                -> OA.OAuth
                -> OA.Credential
                -> a
                -> m OA.Credential
untilAuthorized manager a_lpoauth temp_creds tries = do
    maybe_access_token <- maybeAccessToken manager a_lpoauth temp_creds
    case maybe_access_token of
      Left error_response ->
       if tries > 0
         then do
           liftIO $ threadDelay 1000000 -- Don't spam the server.
           untilAuthorized manager a_lpoauth temp_creds (tries - 1)
         else
           liftIO . throwIO . OA.OAuthException $ "Gaining OAuth Token Credential Failed: " ++ BSL.unpack (responseBody error_response)
      Right access_token -> return access_token

checkFor401 :: W.Status
           -> W.ResponseHeaders
           -> CookieJar
           -> Maybe SomeException
checkFor401 s@(W.Status sci _) hs cookie_jar =
    if sci == 401 || sci == 200
        then Nothing
        else Just $ toException $ StatusCodeException s hs cookie_jar
