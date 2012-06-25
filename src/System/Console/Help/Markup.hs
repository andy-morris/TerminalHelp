-- | Text markup for colouring.
module System.Console.Help.Markup (
    -- ** 'Markup' type
    Markup(..), Chunk(..),
    -- ** Parsing
    parseMarkup, parseMarkup',
    -- ** Other operations
    plainLength, specials
  ) where

import Prelude
import Data.Monoid
import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>))

-- | A chunk of text surrounded with markup characters (or outside them).
data Chunk =
    Plain {chunkValue :: String}        -- ^ No markup
  | Verbatim {chunkValue :: String}     -- ^ @\`verbatim\`@ text
  | Operator {chunkValue :: String}     -- ^ @\@operator\@@ name
  | Placeholder {chunkValue :: String}  -- ^ @|placeholder|@ text
  deriving (Eq, Show)

-- | A passage of marked-up text.
newtype Markup = Markup {unMarkup :: [Chunk]}
  deriving (Eq, Show)

instance Monoid Markup where
  mempty = Markup empty
  Markup m1 `mappend` Markup m2 = Markup $ m1 <|> m2

-- | Length of some text, discarding all markup characters.
plainLength :: Markup -> Int
plainLength = sum . map (length . chunkValue) . unMarkup

-- | Special characters.
--
-- > specials = "`@|"
specials :: [Char]
specials = "`@|"

-- | Wrap some text with the correct constructor for the markup character.
wrap :: Char -> String -> Chunk
wrap '`' = Verbatim
wrap '@' = Operator
wrap '|' = Placeholder
wrap _   = Plain

type P = Parsec String ()

-- | Parse some markup from static text. Calls 'error' in case of any
-- parse errors.
parseMarkup' :: String -> Markup
parseMarkup' str =
  case parseMarkup str of
       Left err -> error $ "parseMarkup':\n" ++ show err
       Right m  -> m

-- | Parse some markup.
parseMarkup :: String -> Either ParseError Markup
parseMarkup = parse markup ""

markup :: P Markup
markup = Markup <$> many chunk

chunk :: P Chunk
chunk = choice [wrapped, Plain <$> many1 plainChar]

wrapped :: P Chunk
wrapped = do
  c <- satisfy (`elem` specials)
  inner <- many plainChar
  _ <- char c
  return $ wrap c inner

plainChar :: P Char
plainChar = choice [escaped, normal] where
  escaped = char '\\' *> anyChar
  normal  = satisfy (`notElem` specials)
