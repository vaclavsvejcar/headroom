{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Headroom.Data.Regex
Description : Helper functions for regular expressions
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Extends functionalify provided by "Text.Regex.PCRE.Light"
and "Text.Regex.PCRE.Heavy" that more suits the needs of this application.
-}

module Headroom.Data.Regex
  ( -- * Data Types
    Regex(..)
  , RegexError(..)
    -- * Regex Functions
  , compile
  , match
  , re
  , replace
  , scan
    -- * Unsafe Functions
  , compileUnsafe
  )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , Value(String)
                                                )
import           Headroom.Types                 ( fromHeadroomError
                                                , toHeadroomError
                                                )
import           Language.Haskell.TH.Quote      ( QuasiQuoter(..) )
import           RIO
import qualified RIO.Text                      as T
import qualified Text.Regex.PCRE.Heavy         as PH
import qualified Text.Regex.PCRE.Light         as PL
import qualified Text.Regex.PCRE.Light.Char8   as PLC


---------------------------------  DATA TYPES  ---------------------------------

-- | Represents compiled /regex/, encapsulates the actual implementation.
newtype Regex = Regex PL.Regex deriving (Eq, Show)

instance FromJSON Regex where
  parseJSON (String s) = pure . compileUnsafe $ s
  parseJSON val = error $ "Invalid value: expected regex, found: " <> show val


------------------------------  PUBLIC FUNCTIONS  ------------------------------


-- | Compiles given /regex/ in /runtime/. If possible, prefer the 're'
-- /quasi quotation/ version that does the same at /compile time/.
compile :: MonadThrow m
        => Text
        -- ^ /regex/ to compile
        -> m Regex
        -- ^ compiled regex
compile raw = either (throwM . CompilationFailed raw . T.pack) pure compile'
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
 where
  quoteExpRegex txt = [| compileUnsafe . T.pack $ txt |]
    where !_ = compileUnsafe . T.pack $ txt -- check at compile time


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


-- | Compiles the given text into /regex/ in runtime. Note that if the /regex/
-- cannot be compiled, it will throw runtime error. Do not use this function
-- unless you know what you're doing.
compileUnsafe :: Text
              -- ^ /regex/ to compile
              -> Regex
              -- ^ compiled /regex/ or runtime exception
compileUnsafe raw = case compile raw of
  Left  err -> error . displayException $ err
  Right res -> res


---------------------------------  Error Types  --------------------------------

-- | Exception specific to the "Headroom.Data.Regex" module.
data RegexError = CompilationFailed Text Text
                -- ^ given input cannot be compiled as /regex/
  deriving (Show, Typeable)

instance Exception RegexError where
  displayException = displayException'
  toException      = toHeadroomError
  fromException    = fromHeadroomError

displayException' :: RegexError -> String
displayException' = T.unpack . \case
  CompilationFailed raw reason ->
    mconcat ["Cannot compile regex from input '", raw, "', reason: ", reason]
