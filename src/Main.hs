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
import qualified Data.Text.IO as Tio

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

type Vocab = Map String TextGenCh

isTextFile :: String -> Bool
isTextFile f = f =~ ".txt$"


loadVocab :: String -> IO Vocab
loadVocab dir = do
  files <- getDirectoryContents dir
  list <- mapM loadFile $ filter isTextFile files
  return $ Map.fromList list
    where loadFile f = do
            gen <- loadOptions ( dir ++ f )
            return ( f, gen )

vocabGet :: Vocab -> String -> TextGenCh
vocabGet v name = case Map.lookup (name ++ ".txt") v of
  Nothing -> word "not found"
  Just gen -> gen

getDir (x:xs) = x
getDir _      = "./"


-- A Remarkable Time Laps Video of [effect] in a [thing]

-- [artwork] in a [location]

-- location = [ adjective, site ("abandoned leather mine") ]

-- [ artist ] [ verbs ] [ artworks ] ( in a [location] | out of [ stuff] )

-- [ artist ] [ transforms ] [ stuff ] into [ adjective ] [ artworks ]

-- [ artist ] uses [ stuff ] to transform [ boringlocation ] into [ magical location ]

-- stuff = [ adjective, including "his/her own" ] [ nouns |  mass noun ]

-- what happens when

-- what a [ large number of ] things look like


crafts :: Vocab -> TextGenCh
crafts v = list [ someone, uses, stuff, to_transform, old, word "into", new ]
  where someone = aan $ list [ perhaps ( 2, 3 ) n, a ]
        vg = vocabGet v
        n = vg "nationality"
        a = vg "artist"
        uses = word "uses"
        stuff = vg "substance"
        to_transform = list [ word "to", vg "transform" ]
        old = aan $ list [ perhaps ( 1, 2 ) (vg "bad_adj"), perhaps (1, 2) (vg "substance"), vg "factory" ]
        new = aan $ list [ perhaps ( 2, 3 ) (vg "good_adj"), vg "magic_site" ]


main :: IO ()
main = do
  args <- getArgs
  v <- loadVocab (getDir args)
  cr <- return $ crafts v
  gf <- return $ runTextGen cr
  res <- getStdRandom gf
  putStrLn $ smartjoin res
