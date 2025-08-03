{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Server.Auth.JWT.DB
  ( JWTConfigDB (..),
    fetchJWTConfigByIssuer,
    processJwtWithDB,
  )
where

import Control.Exception (try)
import Control.Lens
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.JWT qualified as Jose
import Data.Aeson qualified as J
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (newIORef)
import Data.Text qualified as T
import Data.Parser.JSONPath (parseJSONPath)
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

-- | JWT configuration stored in database
data JWTConfigDB = JWTConfigDB
  { jcdIssuer :: !Text,
    jcdJwkUrl :: !(Maybe Text),
    jcdJwk :: !(Maybe J.Value),
    jcdAudience :: !(Maybe Text),
    jcdClaimsNamespace :: !(Maybe Text),
    jcdClaimsNamespacePath :: !(Maybe Text),
    jcdClaimsFormat :: !(Maybe Text),
    jcdClaimsMap :: !(Maybe J.Value),
    jcdAllowedSkew :: !(Maybe Int),
    jcdHeader :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON JWTConfigDB where
  parseJSON = J.genericParseJSON $ J.defaultOptions {J.fieldLabelModifier = drop 3}

instance J.ToJSON JWTConfigDB where
  toJSON = J.genericToJSON $ J.defaultOptions {J.fieldLabelModifier = drop 3}

-- | SQL query to fetch JWT configuration by issuer
fetchJWTConfigQuery :: Text
fetchJWTConfigQuery = 
  "SELECT issuer, jwk_url, jwk, audience, claims_namespace, claims_namespace_path, claims_format, claims_map, allowed_skew, header FROM jwt_config WHERE issuer = $1 LIMIT 1"

-- | Fetch JWT configuration from database by issuer
fetchJWTConfigByIssuer ::
  (MonadIO m, MonadError QErr m, MonadTx m) =>
  Text ->
  m (Maybe JWTConfigDB)
fetchJWTConfigByIssuer issuer = do
  result <- liftTx $ PG.withQE defaultTxErrorHandler (PG.fromText fetchJWTConfigQuery) (issuer, ()) True
  case result of
    [] -> pure Nothing
    [(issuer', jwkUrl, jwk, audience, claimsNs, claimsNsPath, claimsFormat, claimsMap, allowedSkew, header)] -> 
      pure $ Just $ JWTConfigDB issuer' jwkUrl jwk audience claimsNs claimsNsPath claimsFormat claimsMap allowedSkew header
    _ -> throw500 "Multiple JWT configurations found for the same issuer"

-- | Convert database JWT config to JWTConfig
dbConfigToJWTConfig :: JWTConfigDB -> Either Text JWTConfig
dbConfigToJWTConfig JWTConfigDB {..} = do
  -- Parse key or URL
  keyOrUrl <- case (jcdJwk, jcdJwkUrl) of
    (Just jwkJson, Nothing) -> do
      jwk <- case J.fromJSON jwkJson of
        J.Success k -> Right k
        J.Error e -> Left $ "Failed to parse JWK: " <> T.pack e
      Right $ Left jwk
    (Nothing, Just url) -> 
      case parseURI (T.unpack url) of
        Just uri -> Right $ Right uri
        Nothing -> Left $ "Invalid JWK URL: " <> url
    _ -> Left "Either jwk or jwk_url must be specified"

  -- Parse audience
  let audience = jcdAudience >>= \aud ->
        case J.decode (BL.fromStrict $ T.encodeUtf8 $ "[\"" <> aud <> "\"]") of
          Just auds -> Just $ Jose.Audience auds
          Nothing -> Nothing

  -- Parse issuer
  let issuer = StringOrURI <$> Jose.fromString (T.unpack jcdIssuer)

  -- Parse claims configuration
  claims <- case (jcdClaimsMap, jcdClaimsNamespace, jcdClaimsNamespacePath) of
    (Just claimsMapJson, Nothing, Nothing) ->
      case J.fromJSON claimsMapJson of
        J.Success cm -> Right $ JCMap cm
        J.Error e -> Left $ "Failed to parse claims map: " <> T.pack e
    (Nothing, Just ns, Nothing) ->
      Right $ JCNamespace (ClaimNs ns) (parseClaimsFormat jcdClaimsFormat)
    (Nothing, Nothing, Just nsPath) ->
      case parseJSONPath nsPath of
        Right path -> Right $ JCNamespace (ClaimNsPath path) (parseClaimsFormat jcdClaimsFormat)
        Left e -> Left $ "Failed to parse claims namespace path: " <> e
    (Nothing, Nothing, Nothing) ->
      Right $ JCNamespace (ClaimNs defaultClaimsNamespace) defaultClaimsFormat
    _ -> Left "Invalid claims configuration"

  -- Parse allowed skew
  let allowedSkew = fromIntegral <$> jcdAllowedSkew

  -- Parse header
  header <- case jcdHeader of
    Nothing -> Right Nothing
    Just h -> case J.decode (BL.fromStrict $ T.encodeUtf8 h) of
      Just hdr -> Right $ Just hdr
      Nothing -> Left $ "Failed to parse header configuration: " <> h

  Right $ JWTConfig keyOrUrl audience issuer claims allowedSkew header
  where
    parseClaimsFormat Nothing = defaultClaimsFormat
    parseClaimsFormat (Just "json") = JCFJson
    parseClaimsFormat (Just "stringified_json") = JCFStringifiedJson
    parseClaimsFormat _ = defaultClaimsFormat

-- | Process JWT using database configuration
processJwtWithDB ::
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
processJwtWithDB logger httpManager headers mUnAuthRole = do
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
      configMaybe <- fetchJWTConfigByIssuer issuer
      case configMaybe of
        Nothing -> throw400 JWTInvalid $ "No JWT configuration found for issuer: " <> issuer
        Just dbConfig -> do
          -- Convert to JWTConfig
          jwtConfig <- case dbConfigToJWTConfig dbConfig of
            Left err -> throw400 JWTInvalid err
            Right cfg -> pure cfg
          
          -- Create JWTCtx
          jwtCtx <- liftIO $ mkJwtCtxFromConfig jwtConfig
          
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
    
    mkJwtCtxFromConfig :: JWTConfig -> IO JWTCtx
    mkJwtCtxFromConfig JWTConfig {..} = do
      jwkRef <- case jcKeyOrUrl of
        Left jwk -> newIORef (JWKSet [jwk], Nothing)
        Right _ -> newIORef (JWKSet [], Nothing)
      let jwtHeader = fromMaybe JHAuthorization jcHeader
      return $ JWTCtx 
        (either (const Nothing) Just jcKeyOrUrl)
        jwkRef 
        jcAudience 
        jcIssuer 
        jcClaims 
        jcAllowedSkew 
        jwtHeader