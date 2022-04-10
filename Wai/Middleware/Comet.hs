{-# LANGUAGE OverloadedStrings #-}
module Wai.Middleware.Comet (create) where

import Control.Monad
import Network.HTTP.Types
import Network.Wai
import Wai.Middleware.Comet.Base

create :: IO Middleware
create = do
  comet <- new
  pure $ \app req sendResp -> do
    let key = pathInfo req
    let cont = app req $ \resp -> do
          when (isImpureMethod (requestMethod req)
            && statusIsSuccessful (responseStatus resp))
            $ notify comet key
          sendResp resp

    case lookup "X-Comet-Wait" (requestHeaders req) of
      Nothing -> cont
      Just "true" -> wait comet key >> cont
      Just "false" -> cont
      Just s -> sendResp $ responseLBS status400 [] $ "X-Comet-Wait must be true or false"

isImpureMethod :: Method -> Bool
isImpureMethod "POST" = True
isImpureMethod "PUT" = True
isImpureMethod "DELETE" = True
isImpureMethod "PATCH" = True
isImpureMethod _ = False
