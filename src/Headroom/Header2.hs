{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Headroom.Header2
  ( splitSource
  )
where

import           Data.Coerce                         ( coerce )
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
  allLines          = coerce sc :: [CodeLine]
  (middle', after ) = mapT2 SourceCode $ L.splitAt sndSplit allLines
  (before , middle) = mapT2 SourceCode $ L.splitAt fstSplitAt (coerce middle')
  cond              = \ps (lt, t) -> lt == Code && any (`isMatch` t) ps
  fstSplitAt        = maybe 0 ((+ 1) . fst) $ lastMatching (cond fstPs) middle'
  sndSplit          = maybe len fst $ firstMatching (cond sndPs) sc
  len               = length allLines
  mapT2             = join (***)

