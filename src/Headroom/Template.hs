{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Template
  ( parseTemplate
  , parseTemplateFile
  )
where

import           Data.Either.Combinators        ( rightToMaybe )
import           RIO
import qualified RIO.Text                      as T
import           System.IO.Error                ( isDoesNotExistError )
import           Text.Ginger

parseTemplate :: T.Text -> Either ParserError (Template SourcePos)
parseTemplate raw =
  runIdentity $ parseGinger (const $ return Nothing) Nothing (T.unpack raw)

parseTemplateFile :: FilePath -> IO (Either ParserError (Template SourcePos))
parseTemplateFile = parseGingerFile resolve
 where
  resolve path = do
    content <- fmap rightToMaybe (readFileSafe path)
    return $ fmap T.unpack content
  readFileSafe path = tryJust (guard . isDoesNotExistError) (readFileUtf8 path)
