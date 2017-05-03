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
  , loadOptions
  )

type TextGenCh = TextGen StdGen [[Char]]

--type Vocab = Map String TextGenCh

type Vocab = (String -> TextGenCh)

isTextFile :: String -> Bool
isTextFile f = f =~ ".txt$"



loadVocab :: String -> IO Vocab
loadVocab dir = do
  files <- getDirectoryContents dir
  list <- mapM loadFile $ filter isTextFile files
  return $ vocabGet $ Map.fromList list
    where loadFile f = do
            gen <- loadOptions ( dir ++ f )
            return ( f, gen )


vocabGet :: Map String TextGenCh -> String -> TextGenCh
vocabGet v name = case Map.lookup (name ++ ".txt") v of
  Nothing -> word "not found"
  Just gen -> gen

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

-- a utility which runs a generator until it gets two different
-- results

twoDifferent :: TextGenCh -> IO ( [[Char]], [[Char]] )
twoDifferent g = do
  gf <- return $ runTextGen g
  r1 <- getStdRandom gf
  r2 <- iterateUntil (\s -> not $ dumbmatch r1 s) $ do
    getStdRandom gf
  return ( r1, r2 )
  

dumbmatch :: [[Char]] -> [[Char]] -> Bool
dumbmatch r1 r2 = (dumbjoin r1) == (dumbjoin r2)

--
-- Basic components

-- Note: plural artists require us to inflect verbs, which will
-- need some TextGen plumbing

artist :: Vocab -> TextGenCh
artist v = choose [ name, description ]
  where name = list [ v "givenname", v "surname" ]
        description = aan $ list [ n, a ]
        n = v "nationality"
        a = v "artist"

--

aNumberOf :: TextGenCh
aNumberOf = list [ n1, n2 ]
  where n1 = choose $ map word [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ]
        n2 = choose $ map word [ "hundred", "thousand" ]



amountOfStuff :: Vocab -> TextGenCh
amountOfStuff v = list [ aNumberOf, v "measures", word "of", v "substance" ]


subject :: Vocab -> TextGenCh
subject v = choose [ depiction, combination ]
  where depiction = list [ word "of", things, p50 $ list [ word "and", things ] ]
        combination = list [ v "combining", things, word "with", things ]
        things = v "things"

artworks :: Vocab -> TextGenCh
artworks v = list [ artAdj v, choose [ depiction, artwork ] ]
  where depiction = list [ v "artwork", subject v ]
        artwork = choose [ v "artwork", v "artwork_non_rep" ]

artworkSimple :: Vocab -> TextGenCh
artworkSimple v = list [
  artAdj v, choose [ v "artwork", v "artwork_non_rep" ]
  ]

artAdj :: Vocab -> TextGenCh
artAdj v = weighted [
  ( 60, weighted [ ( 60, v "aesthetic"), ( 40, v "good_adj" ) ] ),
  ( 40, list [ v "good_adj", v "aesthetic" ] )
  ]

oldSite :: Vocab -> TextGenCh
oldSite v = choose [ site, factory ]
  where site = aan $ list [ p66 adject, v "site" ]
        factory = aan $ list [ adject, v "substance", v "factory" ]
        adject = choose [ v "bad_adj", v "nationality" ]
        
artSite :: Vocab -> TextGenCh
artSite v = aan $ list [ p66 $ v "good_adj", v "magic_site" ]

artworkInPlace :: Vocab -> TextGenCh
artworkInPlace v = list [ artSite v, v "inrelation", place ]
  where place = choose [ v "city", oldSite v ]


manyArtworks :: Vocab -> TextGenCh
manyArtworks v = list [ aNumberOf, p66 $ artAdj v, v "artwork" ]

manyArtworksInPlace :: Vocab -> TextGenCh
manyArtworksInPlace v = list [
  manyArtworks v,
  word "in",
  choose [ oldSite v, artSite v ]
  ]


videoArt :: Vocab -> TextGenCh
videoArt v = list [ p33 $ v "good_adj", v "footage_adj", v "footage" ]



generalArtwork :: Vocab -> TextGenCh
generalArtwork v = weighted [
  ( 40, artworks v ),
  ( 20, artworkInPlace v ),
  ( 20, manyArtworksInPlace v),
  ( 20, manyArtworks v),
  ( 10, videoArt v )
  ]


artStuff :: Vocab -> TextGenCh
artStuff v = choose [
  v "substance",
  amountOfStuff v,
  list [ p66 $ aNumberOf, v "things" ],
  artworks v,
  manyArtworks v
  ]


-- The next TextGenChs are the alternative sentences

transformsSite :: Vocab -> TextGenCh
transformsSite v = list [ someone, uses, artStuff v, transformation ]
  where someone = artist v
        uses =  word "uses"
        transformation = choose [ o2n, o ]
        o2n = list [ word "to", v "transform", oldSite v, word "into", artSite v ]
        o = list [ word "to transform", oldSite v ]

-- there are different vocab lists for "creates X out of Y" to match
-- verbs with prepositions

transformsThings :: Vocab -> TextGenCh
transformsThings v = choose [ into, outof, with ]
  where a = artist v
        t = v "things"
        works = artworks v
        ws = artworkSimple v
        into = list [ a, v "creates_into", t, word "into", works ]
        outof = list [ a, v "creates_outof", ws, choose [ word "out of", word "from" ], t ]
        with = list [ a, v "creates_with", ws, word "with", t ]


artworksByArtist :: Vocab -> TextGenCh
artworksByArtist v = list [ generalArtwork v, word "by", artist v ]

structureShape :: Vocab -> TextGenCh
structureShape v = choose [ locbefore, locafter ]
  where locbefore = list [ word "in", v "city", word ",", structure ]
        locafter = list [ structure, word "in", v "city" ]
        structure = list [ aan $ v "structure", v "like", aan $ v "thing" ]
        

stuffInPlace :: Vocab -> TextGenCh
stuffInPlace v = list [ amountOfStuff v, p, v "city", ma ]
  where p = choose $ map word [ "in", "above", "below", "outside", "over", "behind" ]
        ma = p50 $ list [ word "by", artist v ]

kerlossus :: Vocab -> TextGenCh
kerlossus v = weighted [
  ( 10, transformsSite v ),
  ( 30, transformsThings v ),
  ( 30, artworksByArtist v ),
  ( 10, stuffInPlace v ),
  ( 20, structureShape v )
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
  cr <- return $ kerlossus v
  gf <- return $ runTextGen cr
  result <- iterateUntil (\s -> length s <= max_length) $ do
    kerlossal <- getStdRandom gf
    return $ mysmartjoin kerlossal
  putStrLn $ renderMarkup $ preEscapedToMarkup $ titlecase $ T.pack result
