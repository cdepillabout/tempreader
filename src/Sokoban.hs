{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sokoban where

import Data.Foldable (fold, foldl', toList)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (groupBy)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector, fromList)
import Numeric.Natural (Natural)
import Text.Parsec
       (ParseError, choice, char, many, many1, optional, optionMaybe, parse)
import Text.Parsec.Text (Parser)
import Text.Pretty.Simple (pPrint)

-------------------------------------------------
-- Tile definition and show and read functions --
-------------------------------------------------

data Pos = Pos
  { posRow :: Natural
  , posCol :: Natural
  } deriving (Eq, Ord, Read, Show)

data TileType
  = Wall
  | Storage
  | Crate
  | Player
  deriving (Eq, Read, Show)

showTileType :: TileType -> Text
showTileType Wall = "#"
showTileType Storage = "."
showTileType Crate = "o"
showTileType Player = "@"

tileTypeParser :: Parser TileType
tileTypeParser =
  choice
    [ char '#' $> Wall
    , char '.' $> Storage
    , char 'o' $> Crate
    , char '@' $> Player
    ]

readTileType :: Text -> Either ParseError TileType
readTileType = parse tileTypeParser ""

data Tile = Tile Pos TileType deriving (Eq, Read, Show)

-------------------------------------------
-- Board def and show and read functions --
-------------------------------------------

newtype Board = Board { unBoard :: Vector Tile } deriving (Eq, Read, Show)

showBoard :: Board -> Text
showBoard (Board vec) = createBoard . toList $ fmap tileToCharAndRow vec
  where
    tileToCharAndRow
      :: Tile
      -> (Natural, Text) -- ^ This is a tuple of the row number (from 0) and
                         -- the 'TileType' shown with 'showTileType'.
    tileToCharAndRow (Tile (Pos row col) tiletype) = (row, showTileType tiletype)

    createBoard :: [(Natural, Text)] -> Text
    createBoard = foldMap rowToText . groupByRow

    groupByRow :: [(Natural, a)] -> [[(Natural, a)]]
    groupByRow = groupBy ((==) `on` fst)

    rowToText :: [(b, Text)] -> Text
    rowToText row = fold (fmap snd row) <> "\n"

rowParser :: Natural -> Parser [Tile]
rowParser rowNum = do
  tileTypes <- many1 tileTypeParser
  let colNums = [0..]
      tileTypesWithColumn = zip colNums tileTypes
      tiles = fmap computeTile tileTypesWithColumn
  pure tiles
  where
    computeTile :: (Natural, TileType) -> Tile
    computeTile (colNum, tileType) = Tile (Pos rowNum colNum) tileType

repeatWithNat :: forall a . (Natural -> Parser a) -> Parser [a]
repeatWithNat f = go [0..] []
  where
    -- | TODO: The nat list should really be represented by an infinite
    -- structure so that we can avoid the error case.  Also, this should
    -- probably be rewritten as an unfold / anamorphism(?) / something else.
    go :: [Natural] -> [Parser a] -> Parser [a]
    go [] _  = error "nat list should never be empty"
    go (nextNat:nats) accum = do
      maybeA <- optionMaybe $ f nextNat
      case maybeA of
        Nothing -> sequence $ reverse accum
        Just a -> go nats (pure a : accum)

-- | 'boardParser' and 'rowParser' are kinda hacky. On hindsight, it probably
-- would have been much easiest to read in all the tiletypes, and then create
-- the Actual list of 'Tile's from that.  Then the 'repeatWithNat' function
-- wouldn't be needed.
boardParser :: Parser Board
boardParser = do
  let rowNums = [0..]
      f nat = rowParser nat <* optional (char '\n')
  allRows <- repeatWithNat f
  pure . Board . fromList $ fold allRows

readBoard :: Text -> Either ParseError Board
readBoard = parse boardParser ""

unsafeReadBoard :: Text -> Board
unsafeReadBoard = either (const $ error "board cannot be parsed") id . readBoard

--------------------
-- Fold functions --
--------------------

foldBoard
  :: Board
  -> (Tile -> [Pos])  -- ^ This is the extraction function for walls.
  -> (Tile -> [Pos])  -- ^ This is the extraction function for storage.
  -> (Tile -> [Pos])  -- ^ This is the extraction function for crates.
  -> ([Pos], [Pos], [Pos])
foldBoard (Board vec) wallExtractor storageExtractor crateExtractor =
  foldl' f ([], [], []) vec
  where
    f :: ([Pos], [Pos], [Pos]) -> Tile -> ([Pos], [Pos], [Pos])
    f (wallAccum, storageAccum, crateAccum) tile =
      ( wallAccum <> wallExtractor tile
      , storageAccum <> storageExtractor tile
      , crateAccum <> crateExtractor tile
      )

foldBoardGeneral
  :: forall a.
     ( Monoid a
     )
  => Board
  -> (Tile -> a)
  -> (Tile -> a)
  -> (Tile -> a)
  -> (a, a, a)
foldBoardGeneral (Board vec) extractor1 extractor2 extractor3 =
  foldl' f (mempty, mempty, mempty) vec
  where
    f :: (a, a, a) -> Tile -> (a, a, a)
    f (accum1, accum2, accum3) tile =
      ( accum1 <> extractor1 tile
      , accum2 <> extractor2 tile
      , accum3 <> extractor3 tile
      )

foldBoardEvenMoreGeneral
  :: forall a.
     ( Monoid a
     )
  => Board
  -> [(Tile -> a)] -- ^ It'd be nice to make this a type-indexed vector,
                   -- because then we could be sure that the return [a] is of
                   -- the same length.
  -> [a]
foldBoardEvenMoreGeneral (Board vec) extractors =
  foldl' f (const mempty <$> extractors) vec
  where
    f :: [a] -> Tile -> [a]
    f accums tile =
      zipWith (\accum extractor -> accum <> extractor tile) accums extractors

getTileType :: TileType -> Tile -> [Pos]
getTileType tileType (Tile pos x)
  | tileType == x = [pos]
  | otherwise = []

getWalls :: Tile -> [Pos]
getWalls = getTileType Wall

getStorages :: Tile -> [Pos]
getStorages = getTileType Storage

getCrates :: Tile -> [Pos]
getCrates = getTileType Crate

testFoldBoard :: IO ()
testFoldBoard = do
  let board = unsafeReadBoard "###\n.o@\n"
      getWalls = getTileType Wall
      getStorages = getTileType Storage
      getCrates = getTileType Crate
      normal = foldBoard board getWalls getStorages getCrates
      general = foldBoardGeneral board getWalls getStorages getCrates
      evenMoreGeneral = foldBoardEvenMoreGeneral board [getWalls, getStorages, getCrates]
  pPrint normal
  pPrint general
  pPrint $ normal == general
  pPrint evenMoreGeneral
