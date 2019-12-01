{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Template
  ( loadTemplate
  , parseTemplate
  , renderTemplate
  )
where

import           Data.Either.Combinators        ( rightToMaybe )
import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T
import           System.IO.Error                ( isDoesNotExistError )
import           Text.Ginger


loadTemplate :: MonadIO m => FilePath -> m (Template SourcePos)
loadTemplate path = do
  parsed <- liftIO $ parseGingerFile resolvePath path
  liftIO $ handleError parsed
 where
  handleError (Left  err) = throwM err
  handleError (Right res) = return res

parseTemplate :: MonadThrow m => T.Text -> m (Template SourcePos)
parseTemplate raw = case result of
  Left  err -> throwM err
  Right res -> return res
 where
  result = runIdentity $ parseGinger noIncludesResolver Nothing (T.unpack raw)

renderTemplate :: HM.HashMap T.Text T.Text -> Template SourcePos -> T.Text
renderTemplate = easyRender

-----------------------------  PRIVATE FUNCTIONS  ------------------------------

noIncludesResolver :: IncludeResolver Identity
noIncludesResolver = const $ return Nothing

resolvePath :: MonadIO m => FilePath -> m (Maybe String)
resolvePath path = do
  content <- fmap rightToMaybe (readFileSafe path)
  return $ fmap T.unpack content
 where
  readFileSafe p =
    liftIO $ tryJust (guard . isDoesNotExistError) (readFileUtf8 p)
