module Main where

import Control.Monad (forM)
import Control.Monad.Loops (iterateUntil)
import Data.List (intercalate)
import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random
import System.Environment (getArgs)
import System.Directory (getDirectoryContents)
import Text.Regex.Posix
import qualified Data.Text as T
import Text.Read (readMaybe)
import Text.Blaze
import Text.Blaze.Renderer.Pretty
import qualified Data.Text.IO as Tio
import Data.Text.Titlecase


import TextGen (
  TextGen
  , Vocab
  , runTextGen
  , word
  , aan
  , choose
  , weighted
  , list
  , randrep
  , perhaps
  , smartjoin
  , dumbjoin
  , upcase
  , postgen
  , loadVocab
  )

type TextGenCh = TextGen StdGen [[Char]]


getDir (x:xs) = x
getDir _      = "./"

-- postprocess to remove the period

mysmartjoin :: [ [Char] ] -> [ Char ]
mysmartjoin xs = take ((length sentence) - 1) sentence
  where sentence = smartjoin xs


-- some shortcuts for probabilities

p50 = perhaps ( 1, 2 )

p33 = perhaps ( 1, 3 )

p66 = perhaps ( 2, 3 )


-- an abbreviation for choosing from vocabs

c :: Vocab -> String -> TextGenCh
c v s = choose $ v s

--
-- Basic components

-- Note: plural artists require us to inflect verbs, which will
-- need some TextGen plumbing

artist :: Vocab -> TextGenCh
artist v = choose [ name, description ]
  where name = list [ c v "givenname", c v "surname" ]
        description = aan $ list [ n, a ]
        n = c v "nationality"
        a = c v "artist"

--

aNumberOf :: TextGenCh
aNumberOf = list [ n1, n2 ]
  where n1 = choose $ map word [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ]
        n2 = choose $ map word [ "hundred", "thousand" ]



amountOfStuff :: Vocab -> TextGenCh
amountOfStuff v = list [ aNumberOf, c v "measures", word "of", c v "substance" ]


-- use choose2 and a do to make sure the things are different
-- add things growing out of, sprouting or swallowing other things

subject' :: Vocab -> TextGenCh
subject' v = choose [ depiction, combination ]
  where depiction = list [ word "of", things, p50 $ list [ word "and", things ] ]
        combination = list [ c v "combining", things, word "with", things ]
        things = c v "things"

--combination :: Vocab -> TextGenCh
--combination v = do
--  ( t1, t2 ) <- choose2 $ vlist $ v "things"

subject :: Vocab -> TextGenCh
subject v = do
  ( t1, t2 ) <- choose (v "things")
  list [ c v "combining", t1, word "and", t2 ]




artworks :: Vocab -> TextGenCh
artworks v = list [ artAdj v, choose [ depiction, artwork ] ]
  where depiction = list [ c v "artwork", subject v ]
        artwork = choose [ c v "artwork", c v "artwork_non_rep" ]

artworkSimple :: Vocab -> TextGenCh
artworkSimple v = list [
  artAdj v, choose [ c v "artwork", c v "artwork_non_rep" ]
  ]

artAdj :: Vocab -> TextGenCh
artAdj v = weighted [
  ( 60, weighted [ ( 60, c v "aesthetic"), ( 40, c v "good_adj" ) ] ),
  ( 40, list [ c v "good_adj", c v "aesthetic" ] )
  ]

oldSite :: Vocab -> TextGenCh
oldSite v = choose [ site, factory ]
  where site = aan $ list [ p66 adject, c v "site" ]
        factory = aan $ list [ adject, c v "substance", c v "factory" ]
        adject = choose [ c v "bad_adj", c v "nationality" ]
        
artSite :: Vocab -> TextGenCh
artSite v = aan $ list [ c v "good_adj", c v "magic_site" ]


artworkInPlace :: Vocab -> TextGenCh
artworkInPlace v = list [ artStuff v, c v "inrelation", place ]
  where place = choose [ c v "city", oldSite v ]


manyArtworks :: Vocab -> TextGenCh
manyArtworks v = list [ aNumberOf, p66 $ artAdj v, c v "artwork" ]

manyArtworksInPlace :: Vocab -> TextGenCh
manyArtworksInPlace v = list [
  manyArtworks v,
  word "in",
  choose [ oldSite v, artSite v ]
  ]

detailedArtworks :: Vocab -> TextGenCh
detailedArtworks v = list [ aesthAdj, form, works ]
  where aesthAdj = p66 $ choose [ c v "good_adj", c v "aesthetic", c v "participle" ]
        form = weighted [ ( 66, quoted $ c v "thing"), ( 33, c v "thing") ] 
        works = choose [ c v "artwork", c v "artwork_non_rep" ]


quoted :: TextGenCh -> TextGenCh
quoted g = postgen q g
  where q ws = [ concat [ "\"", cap $ dumbjoin ws, "\"" ] ] 
        cap []   = []
        cap (c:cs) = toUpper c : cs

videoArt :: Vocab -> TextGenCh
videoArt v = list [ p33 $ c v "good_adj", c v "footage_adj", c v "footage" ]



generalArtwork :: Vocab -> TextGenCh
generalArtwork v = weighted [
  ( 40, artworks v ),
  ( 30, detailedArtworks v ),
  ( 20, artworkInPlace v ),
  ( 20, manyArtworksInPlace v),
  ( 20, manyArtworks v),
  ( 10, videoArt v )
  ]


artStuff :: Vocab -> TextGenCh
artStuff v = choose [
  c v "substance",
  amountOfStuff v,
  list [ p66 $ aNumberOf, c v "things" ],
  artworks v,
  manyArtworks v
  ]


-- The next TextGenChs are the alternative sentences

-- An Artist Uses Materials to transform a Boring Place into a Good Place

transformsSite :: Vocab -> TextGenCh
transformsSite v = list [ someone, uses, artStuff v, transformation ]
  where someone = artist v
        uses =  word "uses"
        transformation = choose [ o2n, o ]
        o2n = list [ word "to", c v "transform", oldSite v, word "into", artSite v ]
        o = list [ word "to transform", oldSite v ]

-- An Artist fills an Old Site with Artworks

fillsSite :: Vocab -> TextGenCh
fillsSite v = list [ artist v, fills, osite, word "with", art ]
  where fills = c v "fills"
        osite = oldSite v
        art = choose [ artworks v, artworkSimple v ]


-- An Artist Transforms Some Things Into Other Things

-- there are different vocab lists for "creates X out of Y" to match
-- verbs with prepositions

transformsThings :: Vocab -> TextGenCh
transformsThings v = choose [ into, outof, with ]
  where a = artist v
        t = c v "things"
        works = artworks v
        ws = artworkSimple v
        into = list [ a, c v "creates_into", t, word "into", works ]
        outof = list [ a, c v "creates_outof", ws, choose [ word "out of", word "from" ], t ]
        with = list [ a, c v "creates_with", ws, word "with", t ]

-- Artworks By Artist

artworksByArtist :: Vocab -> TextGenCh
artworksByArtist v = list [ generalArtwork v, word "by", artist v ]

-- A Thing Shaped Like Another Thing

structureShape :: Vocab -> TextGenCh
structureShape v = choose [ locbefore, locafter ]
  where locbefore = list [ word "in", c v "city", word ",", structure ]
        locafter = list [ structure, word "in", c v "city" ]
        structure = list [ aan $ c v "structure", c v "like", aan $ c v "thing" ]


-- A Bunch of Stuff Near A Place

stuffInPlace :: Vocab -> TextGenCh
stuffInPlace v = list [ amountOfStuff v, c v "inrelation", c v "city", ma ]
  where ma = p50 $ list [ word "by", artist v ]

kerlossus :: Vocab -> TextGenCh
kerlossus v = weighted [
  ( 15, transformsSite v ),
  ( 10, fillsSite v ),
  ( 18, transformsThings v ),
  ( 20, artworksByArtist v ),
  ( 10, stuffInPlace v ),
  ( 5, structureShape v )
  ]

testescape :: Vocab -> TextGenCh
testescape _ = word "Mix & match <hi there> this is bad for HTML'''"
        

default_max_length :: Int
default_max_length = 140

maxLength :: [ String ] -> Int
maxLength (a:b:cs) = case readMaybe b of
  (Just i) -> i
  Nothing  -> default_max_length
maxLength _        = default_max_length

main :: IO ()
main = do
  args <- getArgs
  v <- loadVocab (getDir args)
  max_length <- return $ maxLength args
  result <- iterateUntil (\s -> length s <= max_length) $ do
    kerlossal <- getStdRandom $ runTextGen $ kerlossus v
    return $ mysmartjoin kerlossal
  putStrLn $ renderMarkup $ preEscapedToMarkup $ titlecase $ T.pack result
