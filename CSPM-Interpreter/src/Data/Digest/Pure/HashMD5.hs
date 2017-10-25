{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
--
-- Module      : Data.Digest.Pure.HashMD5
-- License     : BSD3
-- Stability   : experimental
-- Portability : portable, requires bang patterns and ByteString
-- Tested with : GHC-6.12.2
--
-- Use the MD5-rounds to compute hash-values for data-structures.
-----------------------------------------------------------------------------

module Data.Digest.Pure.HashMD5
	(
          MD5Digest
        , Hash(..)
        , md5Init
        , mixRaw
        , mix
        , mix3
        , mix4
        , mix5
        , mixInt
        , foldHash
        ) where

import Data.Digest.Pure.MD5
import Data.Char

{-# INLINE mix5 #-}
mix5 ::
     MD5Digest
  -> MD5Digest
  -> MD5Digest
  -> MD5Digest
  -> MD5Digest
  -> MD5Digest

mix5 a (MD5Digest w0 w1 w2 w3) (MD5Digest w4 w5 w6 w7)
  (MD5Digest w8 w9 w10 w11) (MD5Digest w12 w13 w14 w15)
  = mixRaw a w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15

{-# INLINE mix4 #-}
mix4 :: MD5Digest -> MD5Digest -> MD5Digest -> MD5Digest -> MD5Digest
mix4 a b c d = mix5 md5Init a b c d

{-# INLINE mix3 #-}
mix3 :: MD5Digest -> MD5Digest -> MD5Digest -> MD5Digest
mix3 a b c = mix5 md5Init md5Init a b c

{-# INLINE mix #-}
mix :: MD5Digest -> MD5Digest -> MD5Digest
mix a b = mix5 md5Init md5Init md5Init a b

class Hash a where
  hash :: a -> MD5Digest

-- instance Monoid HashMD5
instance Hash MD5Digest where
  hash = id
 
instance Hash Char where
  hash c = mixRaw md5Init (fromIntegral $ ord c) 234124 23415 3452 0 0 0 0
              0 0 0 0 0 0 0 0

instance (Hash a, Hash b) => Hash (a,b) where
  hash (a, b) = mix (hash a) (hash b)

instance (Hash a, Hash b, Hash c) => Hash (a, b, c) where
  hash (a, b, c) = mix3 (hash a) (hash b) (hash c)

instance (Hash a, Hash b, Hash c, Hash d) => Hash (a, b, c, d) where
  hash (a, b, c, d) = mix4 (hash a) (hash b) (hash c) (hash d)


hashString :: String -> MD5Digest
hashString
  = foldHash $ mixRaw md5Init 234 42 21 23 0 0 0 0 0 0 0 0 0 0 0 0

instance Hash a => Hash [a] where
  hash = foldHash (hashString "Prelude.List standart")

foldHash :: Hash a => MD5Digest -> [a] -> MD5Digest
foldHash !acc [] = acc
foldHash !acc [a] = mix acc (hash a)
foldHash !acc [a,b] = mix3 acc (hash a) (hash b)
foldHash !acc [a,b,c] = mix4 acc (hash a) (hash b) (hash c)
foldHash !acc (a:b:c:d:rest) = foldHash (mix5 acc (hash a) (hash b) (hash c) (hash d)) rest

{-# inline mixInt #-}
mixInt :: MD5Digest -> Int -> MD5Digest
mixInt h i = mixRaw h (fromIntegral i) 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

{-
foldInt :: Hash64 -> [Int] -> Hash64
foldInt !acc [] = acc
foldInt !acc [a] = mixRaw acc (fromIntegral a) 0 0 0
foldInt !acc [a,b] = mixRaw acc (fromIntegral a) (fromIntegral b) 0 0
foldInt !acc [a,b,c]
  = mixRaw acc (fromIntegral a) (fromIntegral b) (fromIntegral c) 0
foldInt !acc [a,b,c,d]
  = mixRaw acc (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
foldInt !acc (a:b:c:d:rest)
  = foldInt
      (mixRaw acc (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d))
      rest
-}
