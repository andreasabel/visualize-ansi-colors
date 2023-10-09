{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Reproduce something like the color table at https://www.thoughtco.com/contrasting-foreground-background-colors-4061363

module Main (main) where

import Data.Foldable ( forM_ )
import Data.List     ( intercalate )
import Data.Map      ( Map )
import Data.Map      qualified as Map

import System.Console.ANSI
  ( Color( Black, White, Red, Blue, Green, Cyan, Magenta, Yellow )
  , ColorIntensity( Dull, Vivid )
  , ConsoleLayer( Background, Foreground )
  , SGR( Reset, SetColor )
  , setSGR
  )

-- | Ansi colors in custom order.
--
colors :: [ Color ]
colors =
  [ Red
  , Yellow
  , Green
  , Blue
  , Cyan
  , Magenta
  , Black
  , White
  ]

-- | Readability scores, in order of declining readability.
--
data Readability = Good | Okay | Poor | None
  deriving (Show)

-- Vivid fg and dull bg
readabilityVividDull0 :: [[Readability]]
readabilityVividDull0 =
  -- Column: background
  --  Red Yellow Green Blue Cyan Magenta Black White
  [ [ Poor, Okay, Good, Poor, Good, None, Okay, Good ] -- fg Red
  , [ Good, Good, Good, Good, Good, Good, Good, Poor ] -- fg Yellow
  , [ Good, Good, Good, Good, Okay, Good, Good, None ] -- fg Green
  , [ Poor, Good, Good, None, Good, Poor, None, Good ] -- fg Blue
  , [ Good, Good, Good, Good, Poor, Good, Good, None ] -- fg Cyan
  , [ Poor, Okay, Good, Poor, Good, Poor, Okay, Good ] -- fg Magenta
  , [ Poor, Okay, Good, Poor, Good, Poor, Okay, Good ] -- fg Black
  , [ Good, Good, Good, Good, Good, Good, Good, Poor ] -- fg White
  ]

-- * The data
------------------------------------------------------------------------

-- Vivid fg and bg
readabilityVividVivid0 :: [[Readability]]
readabilityVividVivid0 =
  -- Column: background
  --  Red Yellow Green Blue Cyan Magenta Black White
  [ [ None, Good, Good, Poor, Good, Poor, Poor, Good ] -- fg Red
  , [ Good, None, Poor, Good, Poor, Good, Good, None ] -- fg Yellow
  , [ Good, Poor, None, Good, Poor, Good, Good, Poor ] -- fg Green
  , [ Poor, Good, Good, None, Good, Poor, Good, Good ] -- fg Blue
  , [ Good, Good, None, Good, None, Good, Good, Poor ] -- fg Cyan
  , [ None, Good, Good, Poor, Good, None, Poor, Good ] -- fg Magenta
  , [ Poor, Good, Good, Poor, Good, Poor, Poor, Good ] -- fg Black
  , [ Good, None, Poor, Good, None, Good, Good, None ] -- fg White
  ]

-- Dull fg and bg
readabilityDullDull0 :: [[Readability]]
readabilityDullDull0 =
  -- Column: background
  --  Red Yellow Green Blue Cyan Magenta Black White
  [ [ None, Good, Good, Poor, Good, Poor, Poor, Good ] -- fg Red
  , [ Good, Poor, Poor, Good, None, Okay, Good, Okay ] -- fg Yellow
  , [ Good, Poor, Poor, Good, None, Poor, Good, Okay ] -- fg Green
  , [ Poor, Good, Good, None, Good, Good, None, Good ] -- fg Blue
  , [ Good, Poor, None, Good, None, Poor, Good, Okay ] -- fg Cyan
  , [ None, Good, Good, Poor, Good, None, Okay, Good ] -- fg Magenta
  , [ Okay, Good, Good, Poor, Good, Good, None, Good ] -- fg Black
  , [ Good, Poor, Poor, Good, Poor, Good, Good, None ] -- fg White
  ]

-- Dull fg and vivid bg
readabilityDullVivid0 :: [[Readability]]
readabilityDullVivid0 =
  -- Column: background
  --  Red Yellow Green Blue Cyan Magenta Black White
  [ [ Poor, Good, Good, Poor, Good, Okay, Good, Good ] -- fg Red
  , [ Poor, Good, Poor, Good, Okay, Poor, Poor, Good ] -- fg Yellow
  , [ Poor, Good, Poor, Good, Okay, Poor, Poor, Good ] -- fg Green
  , [ Good, Good, Good, Poor, Good, Good, Good, Good ] -- fg Blue
  , [ Poor, Good, Poor, Good, Okay, Poor, Poor, Good ] -- fg Cyan
  , [ None, Good, Good, Poor, Good, Poor, Okay, Good ] -- fg Magenta
  , [ Good, Good, Good, Poor, Good, Good, Good, Good ] -- fg Black
  , [ Good, None, None, Good, None, Good, Good, Poor ] -- fg White
  ]

mkReadability :: [[Readability]] -> Map (Color, Color) Readability
mkReadability r = Map.fromList
    [ ((fg, bg), r !! row !! col)
    | (fg, row) <- zip colors [0..]
    , (bg, col) <- zip colors [0..]
    ]

readabilityVividDull :: Map (Color, Color) Readability
readabilityVividDull = mkReadability readabilityVividDull0

readabilityVividVivid :: Map (Color, Color) Readability
readabilityVividVivid = mkReadability readabilityVividVivid0

readabilityDullDull :: Map (Color, Color) Readability
readabilityDullDull = mkReadability readabilityDullDull0

readabilityDullVivid :: Map (Color, Color) Readability
readabilityDullVivid = mkReadability readabilityDullVivid0

-- * Visualizing the data
------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Readability vivid on dull"
  printReadability Vivid Dull readabilityVividDull
  putStrLn ""
  putStrLn "Readability vivid on vivid"
  printReadability Vivid Vivid readabilityVividVivid
  putStrLn ""
  putStrLn "Readability dull on dull"
  printReadability Dull Dull readabilityDullDull
  putStrLn ""
  putStrLn "Readability dull on vivid"
  printReadability Dull Vivid readabilityDullVivid

printReadability :: ColorIntensity -> ColorIntensity -> Map (Color, Color) Readability -> IO()
printReadability ifg ibg r = do
  putStrLn $ mkRow $ "FG \\ BG" : map show colors
  putStrLn $ mkRow $ replicate (length colors + 1) $ replicate width '-'
  forM_ colors \ fg -> do
    putStr $ bar ++ center (show fg)
    forM_ colors \ bg -> do
      putStr bar
      setSGR [SetColor Foreground ifg fg]
      setSGR [SetColor Background ibg bg]
      putStr $ center $ show $ r Map.! (fg, bg)
      setSGR [Reset]
    putStrLn bar

width :: Int
width = 8

center :: String -> String
center s = replicate k1 ' ' ++ s ++ replicate k2 ' '
  where
    k  = width - length s
    k1 = k `div` 2
    k2 = k - k1

mkRow :: [String] -> String
mkRow ss = bar ++ intercalate bar (map center ss) ++ bar

bar :: String
bar = "|"
