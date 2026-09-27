{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading a module's exports and fixities straight out of its interface
-- file, for GHC 9.10, 9.12, and 9.14.
module Tilia.Fixity.HiFile
  ( HiFile (..),
    HiName (..),
    HiExport (..),
    decodeHiFile,
  )
where

import Data.Array (Array, bounds, listArray, (!))
import Data.Bits (shiftL, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Word (Word32, Word8)
import Tilia.Fixity (Direction (..), Fixity (..), Namespace (..), OpName (..))

-- | What an interface file says about the operators its module offers.
data HiFile = HiFile
  { -- | The module the file is the interface of.
    hiModule :: Text,
    -- | What the module exports.
    hiExports :: [HiExport],
    -- | The fixities the module declares.
    hiFixities :: [(Namespace, OpName, Fixity)]
  }
  deriving (Eq, Show)

-- | A name an interface refers to.
data HiName
  = -- | A name with the module that defines it and its namespace.
    HiName Text Namespace OpName
  | -- | A name GHC knows by its unique alone, which only GHC can resolve.
    KnownKey Word32
  deriving (Eq, Show)

-- | One entry of an export list.
data HiExport
  = -- | A name on its own.
    Avail HiName
  | -- | A type or class, and the names that come with it, itself first
    -- where it is exported too.
    AvailTC HiName [HiName]
  deriving (Eq, Show)

-- | Decode an interface file, or say why it cannot be.
decodeHiFile :: ByteString -> Either Text HiFile
decodeHiFile bytes = fst <$> runGet hiFile bytes 0

-- | How the interface files of one GHC series are laid out.
data Series = Series910 | Series912 | Series914
  deriving (Eq)

-- | The strings and names a file refers to by index.
data Tables = Tables
  { tablesStrings :: Array Int ByteString,
    tablesNames :: Array Int (Int, Namespace, Int)
  }

-- | An interface file, from its header as far as its fixities.
hiFile :: Get HiFile
hiFile = do
  magic <- word32be
  unless' (magic == 0x1face64) (failure ("not a 64-bit interface file: " <> T.pack (show magic)))
  version <- string
  series <- case take 3 version of
    "910" -> pure Series910
    "912" -> pure Series912
    "914" -> pure Series914
    _ -> failure ("unsupported interface version " <> T.pack version)
  _way <- string
  unless' (series == Series914) fingerprint
  _extFields <- pointer series
  stringsAt <- pointer series
  namesAt <- pointer series
  unless' (series == Series910) (() <$ pointer series)
  strings <- lookingAt stringsAt dictionary
  names <- lookingAt namesAt symbolTable
  let tables = Tables strings names
  payload series tables

-- | What follows the tables, as far as the fixities.
payload :: Series -> Tables -> Get HiFile
payload series tables = do
  self <- moduleName tables
  _sigOf <- maybeOf (moduleName tables)
  _hscSource <- byte
  case series of
    Series914 -> do
      fingerprint
      skipLazy series
      _public <- pointer series
      exports <- withinLazy series (listOf (export tables))
      skipLazy series
      _ <- pointer series
      fixities <- listOf (fixity series tables)
      pure (HiFile self exports fixities)
    _ -> do
      mapM_ (const fingerprint) [1 :: Int .. 6]
      _orphan <- byte
      _famInsts <- byte
      skipLazy series
      skipLazy series
      exports <- listOf (export tables)
      fingerprint
      _usedTH <- byte
      fixities <- listOf (fixity series tables)
      pure (HiFile self exports fixities)

-- | A pointer, as the absolute position it points to.
pointer :: Series -> Get Int
pointer = \case
  Series910 -> fromIntegral <$> word32be
  _ -> do
    here <- position
    offset <- word32be
    pure (here + fromIntegral offset)

-- | Pass over something written with @lazyPut@.
skipLazy :: Series -> Get ()
skipLazy series = pointer series >>= seek

-- | Read something written with @lazyPut@ and carry on after it.
withinLazy :: Series -> Get a -> Get a
withinLazy series get = do
  end <- pointer series
  a <- get
  seek end
  pure a

-- | The string table.
dictionary :: Get (Array Int ByteString)
dictionary = do
  n <- sleb
  strings <- mapM (const (sleb >>= bytesOf)) [1 .. n]
  pure (listArray (0, n - 1) strings)

-- | The name table, each name as the index of its module's name, its
-- namespace, and the index of its own.
symbolTable :: Get (Array Int (Int, Namespace, Int))
symbolTable = do
  n <- sleb
  names <- mapM (const entry) [1 .. n]
  pure (listArray (0, n - 1) names)
  where
    entry = do
      unit
      m <- uleb
      (namespace, occ) <- occName
      pure (m, namespace, occ)

-- | Look a string up by the index it is written as.
fastString :: Array Int ByteString -> Int -> Get Text
fastString strings i
  | inRange (bounds strings) i = pure (T.decodeUtf8Lenient (strings ! i))
  | otherwise = failure "string index out of range"

-- | Whether an index is within bounds.
inRange :: (Int, Int) -> Int -> Bool
inRange (lo, hi) i = lo <= i && i <= hi

-- | A unit, which must be a real one: instantiated units come of Backpack.
unit :: Get ()
unit =
  byte >>= \case
    0 -> () <$ uleb
    2 -> pure ()
    _ -> failure "an instantiated unit"

-- | An 'OccName', as its namespace and the index of its string.
occName :: Get (Namespace, Int)
occName = do
  namespace <-
    byte >>= \case
      0 -> pure InTerms
      1 -> pure InTerms
      2 -> pure InTypes
      3 -> pure InTypes
      4 -> InTerms <$ uleb
      b -> failure ("unknown namespace " <> T.pack (show b))
  s <- uleb
  pure (namespace, s)

-- | A module, as its name.
moduleName :: Tables -> Get Text
moduleName tables = do
  unit
  uleb >>= fastString (tablesStrings tables)

-- | A reference to a name.
name :: Tables -> Get HiName
name tables = do
  i <- uleb
  let w = fromIntegral i :: Word32
  case w .&. 0xC0000000 of
    0x00000000
      | inRange (bounds (tablesNames tables)) i -> do
          let (m, namespace, occ) = tablesNames tables ! i
          HiName
            <$> fastString (tablesStrings tables) m
            <*> pure namespace
            <*> (OpName <$> fastString (tablesStrings tables) occ)
      | otherwise -> failure "name index out of range"
    0x80000000 -> pure (KnownKey w)
    _ -> failure "unknown name tag"

-- | One entry of an export list.
export :: Tables -> Get HiExport
export tables =
  byte >>= \case
    0 -> Avail <$> name tables
    _ -> AvailTC <$> name tables <*> listOf (name tables)

-- | One declared fixity.
fixity :: Series -> Tables -> Get (Namespace, OpName, Fixity)
fixity series tables = do
  (namespace, s) <- occName
  op <- OpName <$> fastString (tablesStrings tables) s
  case series of
    Series910 ->
      byte >>= \case
        0 -> pure ()
        _ -> () <$ uleb
    _ -> pure ()
  precedence <- sleb
  direction <-
    byte >>= \case
      0 -> pure LeftAssoc
      1 -> pure RightAssoc
      _ -> pure NoAssoc
  pure (namespace, op, Fixity direction precedence)

-- | A 'Fingerprint', which is two numbers.
fingerprint :: Get ()
fingerprint = () <$ uleb <* uleb

-- | A 'String', which is a list of characters.
string :: Get String
string = listOf (toEnum <$> uleb)

-- | A 'Maybe'.
maybeOf :: Get a -> Get (Maybe a)
maybeOf get =
  byte >>= \case
    0 -> pure Nothing
    _ -> Just <$> get

-- | A list, which is its length followed by its elements.
listOf :: Get a -> Get [a]
listOf get = do
  n <- sleb
  mapM (const get) [1 .. n]

-- | Run an action unless the condition holds.
unless' :: Bool -> Get () -> Get ()
unless' condition action = if condition then pure () else action

-- | Reading from a position in a byte string.
newtype Get a = Get{runGet :: ByteString -> Int -> Either Text (a, Int)}

instance Functor Get where
  fmap f (Get g) = Get $ \bs i -> case g bs i of
    Left e -> Left e
    Right (a, j) -> Right (f a, j)

instance Applicative Get where
  pure a = Get $ \_ i -> Right (a, i)
  Get f <*> Get g = Get $ \bs i -> case f bs i of
    Left e -> Left e
    Right (h, j) -> case g bs j of
      Left e -> Left e
      Right (a, k) -> Right (h a, k)

instance Monad Get where
  Get g >>= f = Get $ \bs i -> case g bs i of
    Left e -> Left e
    Right (a, j) -> runGet (f a) bs j

-- | Stop reading, and say why.
failure :: Text -> Get a
failure e = Get $ \_ _ -> Left e

-- | Where reading has got to.
position :: Get Int
position = Get $ \_ i -> Right (i, i)

-- | Carry on reading from a position.
seek :: Int -> Get ()
seek j = Get $ \bs _ ->
  if j < 0 || j > BS.length bs
    then Left "pointer out of range"
    else Right ((), j)

-- | Run something at a position and come back.
lookingAt :: Int -> Get a -> Get a
lookingAt j get = do
  here <- position
  seek j
  a <- get
  seek here
  pure a

-- | One byte.
byte :: Get Word8
byte = Get $ \bs i ->
  if i < BS.length bs
    then Right (BS.unsafeIndex bs i, i + 1)
    else Left "unexpected end of file"

-- | So many bytes, as they are.
bytesOf :: Int -> Get ByteString
bytesOf n = Get $ \bs i ->
  if n >= 0 && i + n <= BS.length bs
    then Right (BS.unsafeTake n (BS.unsafeDrop i bs), i + n)
    else Left "unexpected end of file"

-- | A 32-bit word, most significant byte first.
word32be :: Get Word32
word32be = do
  a <- byte
  b <- byte
  c <- byte
  d <- byte
  pure (fromIntegral a `shiftL` 24 .|. fromIntegral b `shiftL` 16 .|. fromIntegral c `shiftL` 8 .|. fromIntegral d)

-- | An unsigned LEB128 number.
uleb :: Get Int
uleb = go 0 0
  where
    go shift acc = do
      b <- byte
      let acc' = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` shift)
      if testBit b 7 then go (shift + 7) acc' else pure acc'

-- | A signed LEB128 number.
sleb :: Get Int
sleb = go 0 0
  where
    go shift acc = do
      b <- byte
      let acc' = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` shift)
          shift' = shift + 7
      if testBit b 7
        then go shift' acc'
        else pure (if testBit b 6 then acc' - (1 `shiftL` shift') else acc')
