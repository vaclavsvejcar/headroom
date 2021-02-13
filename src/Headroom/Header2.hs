{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Headroom.Header2
  ( -- * Copyright Header Detection
    findHeader
  , findBlockHeader
  , findLineHeader
  , splitSource
  )
where

import           Data.Coerce                         ( coerce )
import           Headroom.Configuration.Types        ( CtHeaderConfig
                                                     , HeaderConfig(..)
                                                     , HeaderSyntax(..)
                                                     )
import           Headroom.Data.Regex                 ( Regex
                                                     , isMatch
                                                     )
import           Headroom.SourceCode                 ( CodeLine
                                                     , LineType(..)
                                                     , SourceCode(..)
                                                     , firstMatching
                                                     , lastMatching
                                                     )
import           RIO
import qualified RIO.List                           as L
import qualified RIO.Text                           as T


-- | Finds header position in given text, where position is represented by
-- line number of first and last line of the header (numbered from zero).
-- Based on the 'HeaderSyntax' specified in given 'HeaderConfig', this function
-- delegates its work to either 'findBlockHeader' or 'findLineHeader'.
--
-- >>> :set -XFlexibleContexts -XTypeFamilies -XQuasiQuotes
-- >>> import Headroom.Data.Regex (re)
-- >>> let hc = HeaderConfig ["hs"] 0 0 0 0 [] [] (BlockComment [re|^{-|] [re|(?<!#)-}$|] Nothing)
-- >>> findHeader hc $ SourceCode [(Code, "foo"), (Code, "bar"), (Comment, "{- HEADER -}")]
-- Just (2,2)
findHeader :: CtHeaderConfig
           -- ^ appropriate header configuration
           -> SourceCode
           -- ^ text in which to detect the header
           -> Maybe (Int, Int)
           -- ^ header position @(startLine, endLine)@
findHeader HeaderConfig {..} input = case hcHeaderSyntax of
  BlockComment start end _ -> findBlockHeader start end headerArea splitAt
  LineComment prefix _     -> findLineHeader prefix headerArea splitAt
 where
  (before, headerArea, _) = splitSource hcPutAfter hcPutBefore input
  splitAt                 = length (coerce before :: [CodeLine])


-- | Finds header in the form of /multi-line comment/ syntax, which is delimited
-- with starting and ending pattern.
--
-- >>> :set -XQuasiQuotes
-- >>> import Headroom.Data.Regex (re)
-- >>> let sc = SourceCode [(Code, ""), (Comment, "{- HEADER -}"), (Code, ""), (Code,"")]
-- >>> findBlockHeader [re|^{-|] [re|(?<!#)-}$|] sc 0
-- Just (1,1)
findBlockHeader :: Regex
                -- ^ starting pattern (e.g. @{-@ or @/*@)
                -> Regex
                -- ^ ending pattern (e.g. @-}@ or @*/@)
                -> SourceCode
                -- ^ source code in which to detect the header
                -> Int
                -- ^ line number offset (adds to resulting position)
                -> Maybe (Int, Int)
                -- ^ header position @(startLine + offset, endLine + offset)@
findBlockHeader start end sc offset = mapT2 (+ offset) <$> position
 where
  ls          = zip [0 ..] $ coerce sc
  isMatch'    = \p t -> isMatch p . T.strip $ t
  allComments = all (\(_, (lt, _)) -> lt == Comment)
  hasStart    = maybe False (\(_, (_, t)) -> isMatch' start t) . L.headMaybe
  hasEnd      = maybe False (\(_, (_, t)) -> isMatch' end t) . L.lastMaybe
  position    = (,) <$> (header >>= L.headMaybe) <*> (header >>= L.lastMaybe)
  header =
    (fmap . fmap) fst
      . L.find (\g -> allComments g && hasStart g && hasEnd g)
      . L.groupBy (\(_, (lt1, _)) (_, (lt2, _)) -> lt1 == lt2)
      $ ls


-- | Finds header in the form of /single-line comment/ syntax, which is
-- delimited with the prefix pattern.
--
-- >>> :set -XQuasiQuotes
-- >>> import Headroom.Data.Regex (re)
-- >>> let sc = SourceCode [(Code, ""), (Code, "a"), (Comment, "-- first"), (Comment, "-- second"), (Code, "foo")]
-- >>> findLineHeader [re|^--|] sc 0
-- Just (2,3)
findLineHeader :: Regex
               -- ^ prefix pattern (e.g. @--@ or @//@)
               -> SourceCode
               -- ^ source code in which to detect the header
               -> Int
               -- ^ line number offset (adds to resulting position)
               -> Maybe (Int, Int)
               -- ^ header position @(startLine + offset, endLine + offset)@
findLineHeader prefix sc offset = mapT2 (+ offset) <$> position
 where
  ls       = zip [0 ..] $ coerce sc
  isMatch' = \p t -> isMatch p . T.strip $ t
  position = (,) <$> (header >>= L.headMaybe) <*> (header >>= L.lastMaybe)
  header =
    (fmap . fmap) fst
      . L.find (all (\(_, (lt, t)) -> lt == Comment && isMatch' prefix t))
      . L.groupBy (\(_, (lt1, _)) (_, (lt2, _)) -> lt1 == lt2)
      $ ls


-- | Splits input source code into three parts:
--
--     1. all lines located before the very last occurence of one of the
--        conditions from the first condition list
--     2. all lines between the first and last lists
--     3. all lines located after the very first occurence of one of the
--        conditions from the second condition list
--
-- If both first and second patterns are empty, then all lines are returned in
-- the middle part.
--
-- >>> :set -XQuasiQuotes
-- >>> import Headroom.Data.Regex (re)
--
-- >>> let ls = [(Code, "text"), (Code, "->"), (Code, "RESULT"), (Code, "<-"), (Code, "foo")]
-- >>> splitSource [[re|->|]] [[re|<-|]] $ SourceCode ls
-- (SourceCode [(Code,"text"),(Code,"->")],SourceCode [(Code,"RESULT")],SourceCode [(Code,"<-"),(Code,"foo")])
--
-- >>> let ls = [(Code, "text"), (Code, "->"), (Code, "RESULT"), (Code, "<-"), (Code, "foo")]
-- >>> splitSource [] [[re|<-|]] $ SourceCode ls
-- (SourceCode [],SourceCode [(Code,"text"),(Code,"->"),(Code,"RESULT")],SourceCode [(Code,"<-"),(Code,"foo")])
--
-- >>> splitSource [] [] $ SourceCode [(Code,"foo"), (Code,"bar")]
-- (SourceCode [],SourceCode [(Code,"foo"),(Code,"bar")],SourceCode [])
splitSource :: [Regex]
            -> [Regex]
            -> SourceCode
            -> (SourceCode, SourceCode, SourceCode)
splitSource []    []    sc = (mempty, sc, mempty)
splitSource fstPs sndPs sc = (before, middle, after)
 where
  allLines          = coerce sc
  (middle', after ) = mapT2 SourceCode $ L.splitAt sndSplit allLines
  (before , middle) = mapT2 SourceCode $ L.splitAt fstSplitAt (coerce middle')
  cond              = \ps (lt, t) -> lt == Code && any (`isMatch` t) ps
  fstSplitAt        = maybe 0 ((+ 1) . fst) $ lastMatching (cond fstPs) middle'
  sndSplit          = maybe len fst $ firstMatching (cond sndPs) sc
  len               = length allLines


mapT2 :: (a -> b) -> (a, a) -> (b, b)
mapT2 = join (***)
