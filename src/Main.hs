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
  , list
  , randrep
  , perhaps
  , smartjoin
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

-- p50 = perhaps ( 1, 2 )

-- p33 = perhaps ( 1, 3 )

p66 = perhaps ( 2, 3 )

artist :: Vocab -> TextGenCh
artist v = choose [ name, description ]
  where name = list [ v "givenname", v "surname" ]
        description = aan $ list [ n, a ]
        n = v "nationality"
        a = v "artist"

artworks :: Vocab -> TextGenCh
artworks v = list [ v "aesthetic", v "artwork" ]

oldSite :: Vocab -> TextGenCh
oldSite v = choose [ site, factory ]
  where site = aan $ list [ p66 adject, v "site" ]
        factory = aan $ list [ adject, v "substance", v "factory" ]
        adject = choose [ v "bad_adj", v "nationality" ]
        
artSite :: Vocab -> TextGenCh
artSite v = aan $ list [ p66 $ v "good_adj", v "magic_site" ]

-- The next TextGenChs are the alternative sentences

transformSite :: Vocab -> TextGenCh
transformSite v = list [ someone, uses, stuff, to_transform, old, word "into", new ]
  where someone = artist v
        uses =  word "uses"
        stuff = choose [ v "substance", aan $ v "thing", v "things" ]
        to_transform = list [ word "to", v "transform" ]
        old = oldSite v
        new = artSite v


transformThings :: Vocab -> TextGenCh
transformThings v = list [ artist v, v "creates", v "things", word "into", artworks v ]


artworksByArtist :: Vocab -> TextGenCh
artworksByArtist v = list [ artworks v, word "by", artist v ]

structureShape :: Vocab -> TextGenCh
structureShape v = choose [ locbefore, locafter ]
  where locbefore = list [ word "in", v "city", word ",", structure ]
        locafter = list [ structure, word "in", v "city" ]
        structure = list [ aan $ v "structure", v "like", aan $ v "thing" ]
        

artworkInPlace :: Vocab -> TextGenCh
artworkInPlace v = aan $ list [ artsite v, v "inrelation", place ]
  where place = choose [ v "city", oldSite ]



kerlossus :: Vocab -> TextGenCh
kerlossus v = choose [ s1, s2, s3, s4, s5 ]
  where s1 = transformSite v
        s2 = transformThings v
        s3 = artworksByArtist v
        s4 = structureShape v
        s5 = artworkInPlace v

        

max_length :: Int
max_length = 140

main :: IO ()
main = do
  args <- getArgs
  v <- loadVocab (getDir args)
  cr <- return $ kerlossus v
  gf <- return $ runTextGen cr
  result <- iterateUntil (\s -> length s <= max_length) $ do
    kerlossal <- getStdRandom gf
    return $ mysmartjoin kerlossal
  putStrLn $ renderMarkup $ toMarkup $ titlecase $ T.pack result
