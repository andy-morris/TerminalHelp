-- | Library for writing help text.
module System.Console.Help (
    -- * Colours
    Colors(..), defaultColors, setColor,

    -- * Printing
    output, outputLn,

    -- * Markup
    module System.Console.Help.Markup,
  ) where

import System.Console.Help.Markup
import System.Console.ANSI

-- | A list of SGR commands for each kind of text.
data Colors =
  Colors {plainColor, verbatimColor, operatorColor, placeholderColor :: [SGR]}

-- | Default colors:
--
-- * 'Plain': 'Reset'
--
-- * 'Verbatim': 'Blue'
--
-- * 'Operator': 'Green'
--
-- * 'Placeholder': 'Yellow'
defaultColors :: Colors
defaultColors = Colors [] [setColor Blue] [setColor Green] [setColor Yellow]

sgrFor :: Chunk -> Colors -> [SGR]
sgrFor Plain{}       = plainColor
sgrFor Verbatim{}    = verbatimColor
sgrFor Operator{}    = operatorColor
sgrFor Placeholder{} = placeholderColor

-- | Convenience function for the 'SGR' of a foreground colour.
setColor :: Color -> SGR
setColor = SetColor Foreground Vivid

-- | Prints some marked-up text to stdout.
output :: Colors -> Markup -> IO ()
output cols = mapM_ outputChunk . unMarkup where
  outputChunk c = do
    setSGR (sgrFor c cols)
    putStr $ chunkValue c
    setSGR [Reset]

-- | Prints some marked-up text to stdout, followed by a newline.
outputLn :: Colors -> Markup -> IO ()
outputLn c x = output c x >> putStrLn ""
