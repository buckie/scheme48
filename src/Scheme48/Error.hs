module Scheme48.Error
  (trapError, extractValue)
  where

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue (Right v) = v
extractValue (Left s) = error $ "This should never happen: " ++ show s


