{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Template
  ( Template(..)
  , loadTemplate
  )
where

import           Headroom.FileSystem            ( loadFile )
import           RIO
import qualified RIO.HashMap                   as HM
import           RIO.Text                       ( Text )
import qualified RIO.Text                      as T


class Template t where
    templateExtensions :: proxy t -> [Text]
    parseTemplate :: MonadThrow m => Maybe Text -> Text -> m t
    renderTemplate :: MonadThrow m => HM.HashMap Text Text -> t -> m Text

loadTemplate :: (MonadIO m, Template t) => FilePath -> m t
loadTemplate path = do
  raw <- loadFile path
  liftIO $ parseTemplate (Just $ T.pack path) raw

