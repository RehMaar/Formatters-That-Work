{- From Haskell2010

importdecl -> import [qualified] modid [as modid] [impspec]

impspec -> (import1, .. importn) | hiding (import1,..importn)

import -> var
        | typecon [ (..) | (cname1, .. cnamen) ]
        | tycls   [ (..) | (var1, .. varn) ]

cname = var | con

-}

{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving     #-}
{-# LANGUAGE TypeOperators, PatternSynonyms           #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts      #-}

import HsSyn

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Pos
import Text.Parsec.ByteString
import Data.Functor
import Data.Char
import Data.Maybe

-- Helper
betweenSpaces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenSpaces = between spaces spaces

betweenParan :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenParan = between (char '(') (char ')')

-- Syntex parts
importLabel = "import"
quilifiedLabel = "qualified"
hidingLabel = "hiding"

allowedIDChar :: Stream s m Char => ParsecT s u m Char
allowedIDChar = alphaNum <|> oneOf "!#$%&*+./<=>?@\\^|-~:"

-- Parse imports
-- import _QUILIFIED_
hasQualified :: Stream s m Char => ParsecT s u m Bool
hasQualified = isJust <$> optionMaybe (string quilifiedLabel)

-- import [qualified] _NAME_
symbName :: Stream s m Char => ParsecT s u m String
symbName = many1 (alphaNum <|> char '.')


-- Shortcuts
importShortcut :: Stream s m Char => ParsecT s u m (Maybe String)
importShortcut = optionMaybe (string "as" *> betweenSpaces (many1 letter))


hasHiding : :Stream s m Char => ParsecT s u m String
hasHiding = isJust <$> optionMaybe (string hidingLabel)

specImportSymbol :: Stream s m Char => ParsecT s u m String
specImportSymbol = (string "(") <> many1 allowedIDChar <> (string ")")

listImportedSymbols :: Stream s m Char => ParsecT s u m (Maybe [String])
listImportedSymbols = betweenParan $ betweenSpaces $ importSymbol

listImportedSymbols :: Stream s m Char => ParsecT s u m String
listImportedSymbols = undefined

{-
importSymbols :: Stream s m Char => ParsecT s u m String
importSymbols = specImportSymbol <|> symbName

-- Import Spec
listImportedSymbols :: Stream s m Char => ParsecT s u m (Maybe [String])
listImportedSymbols = optionMaybe $ betweenParan $ sepEndBy importSymbols (betweenSpaces (char ','))

-- import declaration
imports :: Stream s m Char => ParsecT s u m Import
imports = string importLabel *> spaces *> (flip Import <$> hasQualified <*> (betweenSpaces symbName) <*> importShortcut <*> listImportedSymbols)

test1 = "import qualified Data.List as DL (sort, sortBy, (<|>))"
test2 = "import qualified Data.List as DL hiding (sort, sortBy, (<|>))"
test3 = "import Data.Tree (Tree(Node), levels)"
-}
