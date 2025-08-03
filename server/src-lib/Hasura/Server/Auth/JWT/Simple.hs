{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Simplified JWT processing with database backend
module Hasura.Server.Auth.JWT.Simple
  ( processJwtWithDatabase,
    createJWTCtxFromDB,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.JWT qualified as Jose
import Data.Aeson qualified as J
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.IORef (newIORef)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Database.PG.Query qualified as PG
import Hasura.Authentication.Role (RoleName)
import Hasura.Authentication.User (UserInfo)
import Hasura.Base.Error
import Hasura.Backends.Postgres.Connection.MonadTx
import Hasura.Logging (Hasura, Logger)
import Hasura.Prelude
import Hasura.Server.Auth.JWT
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as N
import Network.URI (parseURI)

-- | Process JWT with database configuration lookup
processJwtWithDatabase ::
  ( MonadIO m,
    MonadError QErr m,
    MonadTx m,
    MonadBaseControl IO m
  ) =>
  Logger Hasura ->
  HTTP.Manager ->
  HTTP.RequestHeaders ->
  Maybe RoleName ->
  m (UserInfo, Maybe UTCTime, [N.Header], Maybe JWTCtx)
processJwtWithDatabase logger httpManager headers mUnAuthRole = do
  -- Extract JWT from headers
  rawJWT <- extractJWT headers
  
  -- Decode JWT without verification to get issuer
  issuerMaybe <- case tokenIssuer rawJWT of
    Just (StringOrURI iss) -> pure $ Just $ Jose.toText iss
    Nothing -> pure Nothing
  
  case issuerMaybe of
    Nothing -> throw400 JWTInvalid "JWT does not contain issuer claim"
    Just issuer -> do
      -- Fetch JWT config from database
      configResult <- fetchJWTConfigFromDB issuer
      case configResult of
        Nothing -> throw400 JWTInvalid $ "No JWT configuration found for issuer: " <> issuer
        Just dbConfig -> do
          -- Create JWTCtx from database config
          jwtCtx <- liftIO $ createJWTCtxFromDB dbConfig
          
          -- Process JWT with the context
          processJwt [jwtCtx] headers mUnAuthRole
  where
    extractJWT :: (MonadError QErr m) => HTTP.RequestHeaders -> m RawJWT
    extractJWT hdrs = do
      let authHeader = lookup "Authorization" hdrs
      case authHeader of
        Just h -> case BC.words h of
          ["Bearer", jwt] -> pure $ RawJWT $ BL.fromStrict jwt
          _ -> throw400 InvalidHeaders "Malformed Authorization header"
        Nothing -> throw400 InvalidHeaders "Missing Authorization header"

-- | Fetch JWT configuration from database
fetchJWTConfigFromDB ::
  (MonadIO m, MonadError QErr m, MonadTx m) =>
  Text ->
  m (Maybe J.Value)
fetchJWTConfigFromDB issuer = do
  let query = PG.fromText "SELECT config FROM jwt_config WHERE issuer = $1 LIMIT 1"
  result <- liftTx $ PG.withQE defaultTxErrorHandler query (issuer, ()) True
  case result of
    [] -> pure Nothing
    [(PG.ViaJSON config)] -> pure $ Just config
    _ -> throw500 "Multiple JWT configurations found for the same issuer"

-- | Create JWTCtx from database configuration
createJWTCtxFromDB :: J.Value -> IO JWTCtx
createJWTCtxFromDB configJson = do
  case J.fromJSON configJson of
    J.Success jwtConfig -> do
      jwkRef <- case jcKeyOrUrl jwtConfig of
        Left jwk -> newIORef (JWKSet [jwk], Nothing)
        Right _ -> newIORef (JWKSet [], Nothing)
      let jwtHeader = fromMaybe JHAuthorization (jcHeader jwtConfig)
      return $ JWTCtx 
        (either (const Nothing) Just (jcKeyOrUrl jwtConfig))
        jwkRef 
        (jcAudience jwtConfig)
        (jcIssuer jwtConfig)
        (jcClaims jwtConfig)
        (jcAllowedSkew jwtConfig)
        jwtHeader
    J.Error err -> error $ "Failed to parse JWT config from database: " <> err