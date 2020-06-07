{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Headroom.Regex
Description : Helper functions for regular expressions
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Extends functionalify provided by "Text.Regex.PCRE.Light"
and "Text.Regex.PCRE.Heavy" that more suits the needs of this application.
-}

module Headroom.Regex
  ( compile
  , match
  , re
  , scan
  , replace
  , Regex(..)
  , CompileException(..)
  )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , Value(String)
                                                )
import           Language.Haskell.TH     hiding ( match )
import           Language.Haskell.TH.Quote      ( QuasiQuoter(..) )
import           RIO
import qualified RIO.Text                      as T
import qualified Text.Regex.PCRE.Heavy         as PH
import qualified Text.Regex.PCRE.Light         as PL
import qualified Text.Regex.PCRE.Light.Char8   as PLC


-- | Exception type specifying that given input cannot be compiled as /regex/.
data CompileException = CompileException !Text !Text deriving (Show, Typeable)

instance Exception CompileException


-- | Represents compiled /regex/, encapsulates the actual implementation.
newtype Regex = Regex PL.Regex deriving (Eq, Show)

instance FromJSON Regex where
  parseJSON (String s) = pure . compileUnsafe $ s
  parseJSON val = error $ "Invalid value: expected regex, found: " <> show val


-- | Compiles given /regex/ in /runtime/. If possible, prefer the 're'
-- /quasi quotation/ version that does the same at /compile time/.
compile :: MonadThrow m
        => Text
        -- ^ /regex/ to compile
        -> m Regex
        -- ^ compiled regex
compile raw = either (throwM . CompileException raw . T.pack) pure compile'
  where compile' = Regex <$> PH.compileM (encodeUtf8 raw) [PLC.utf8]


-- | Same as 'match', but works with 'Text' and uses no additional options.
match :: Regex
      -- ^ a PCRE regular expression value produced by compile
      -> Text
      -- ^ the subject text to match against
      -> Maybe [Text]
      -- ^ the result value
match (Regex r) subject = fmap T.pack <$> PLC.match r (T.unpack subject) []


-- | A QuasiQuoter for regular expressions that does a compile time check.
re :: QuasiQuoter
re = QuasiQuoter { quoteExp  = quoteExpRegex
                 , quotePat  = undefined
                 , quoteType = undefined
                 , quoteDec  = undefined
                 }


-- | Replaces all occurences of given /regex/.
replace :: Regex
        -- ^ /regex/ to match what to replace
        -> (Text -> [Text] -> Text)
        -- ^ replacement function (as @fullMatch -> [groups] -> result@)
        -> Text
        -- ^ text to replace in
        -> Text
        -- ^ resulting text
replace (Regex regex) = PH.gsub regex


-- | Searches the text for all occurences of given /regex/.
scan :: Regex
     -- ^ /regex/ to search for
     -> Text
     -- ^ input text
     -> [(Text, [Text])]
     -- ^ found occurences (as @[(fullMatch, [groups])]@)
scan (Regex regex) = PH.scan regex


------------------------------  PRIVATE FUNCTIONS  -----------------------------


compileUnsafe :: Text -> Regex
compileUnsafe raw = case compile raw of
  Left  err -> error . displayException $ err
  Right res -> res


quoteExpRegex :: String -> ExpQ
quoteExpRegex txt = [| compileUnsafe . T.pack $ txt |]
  where !_ = compileUnsafe . T.pack $ txt -- check at compile time

