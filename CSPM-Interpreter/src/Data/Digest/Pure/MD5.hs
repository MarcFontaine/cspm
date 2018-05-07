{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- Module      : Data.Digest.Pure.MD5
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : portable, requires bang patterns and ByteString
-- Tested with : GHC-6.8.1
--
-- This is taken from the pureMD5 package but stripped down to remove some dependencies.
-- Ideally one would extend pureMD5 or move this to a seperate package.
-- Original Author is  : Thomas.DuBuisson@gmail.com
--
-----------------------------------------------------------------------------

module Data.Digest.Pure.MD5
  (
    MD5Digest (..)
  , mixRaw
  , md5Init
  ) where

import Data.Word
import Data.Bits

data MD5Digest = MD5Digest !Word32 !Word32 !Word32 !Word32
    deriving (Ord, Eq)

md5Init :: MD5Digest
md5Init = MD5Digest h0 h1 h2 h3
  where
    h0 = 0x67452301
    h1 = 0xEFCDAB89
    h2 = 0x98BADCFE
    h3 = 0x10325476

{-# INLINE applyMD5RoundsRaw #-}
applyMD5RoundsRaw ::
       MD5Digest
    -> Word32 -> Word32 -> Word32 -> Word32
    -> Word32 -> Word32 -> Word32 -> Word32
    -> Word32 -> Word32 -> Word32 -> Word32
    -> Word32 -> Word32 -> Word32 -> Word32
    -> MD5Digest
applyMD5RoundsRaw
    (MD5Digest da db dc dd)
    !w0 !w1 !w2 !w3 
    !w4 !w5 !w6 !w7
    !w8 !w9 !w10 !w11 
    !w12 !w13 !w14 !w15
    =  {-# SCC "applyMD5RoundsRaw" #-}
        let -- Round 1
            !r0  = ff da dd dc dd   w0  7  3614090360
            !r1  = ff dd r0 db dc   w1  12 3905402710
            !r2  = ff dc r1 r0 db   w2  17 606105819
            !r3  = ff db r2 r1 r0   w3  22 3250441966
            !r4  = ff r0 r3 r2 r1   w4  7  4118548399
            !r5  = ff r1 r4 r3 r2   w5  12 1200080426
            !r6  = ff r2 r5 r4 r3   w6  17 2821735955
            !r7  = ff r3 r6 r5 r4   w7  22 4249261313
            !r8  = ff r4 r7 r6 r5   w8  7  1770035416
            !r9  = ff r5 r8 r7 r6   w9  12 2336552879
            !r10 = ff r6 r9 r8 r7  w10 17 4294925233
            !r11 = ff r7 r10 r9 r8 w11 22 2304563134
            !r12 = ff r8 r11 r10 r9 w12 7  1804603682
            !r13 = ff r9 r12 r11 r10 w13 12 4254626195
            !r14 = ff r10 r13 r12 r11 w14 17 2792965006
            !r15 = ff r11 r14 r13 r12 w15 22 1236535329
            -- Round 2
            !r16 = gg r12 r15 r14 r13 w1  5  4129170786
            !r17 = gg r13 r16 r15 r14 w6  9  3225465664
            !r18 = gg r14 r17 r16 r15 w11 14 643717713
            !r19 = gg r15 r18 r17 r16 w0  20 3921069994
            !r20 = gg r16 r19 r18 r17 w5  5  3593408605
            !r21 = gg r17 r20 r19 r18 w10 9  38016083
            !r22 = gg r18 r21 r20 r19 w15 14 3634488961
            !r23 = gg r19 r22 r21 r20 w4  20 3889429448
            !r24 = gg r20 r23 r22 r21 w9  5  568446438
            !r25 = gg r21 r24 r23 r22 w14 9  3275163606
            !r26 = gg r22 r25 r24 r23 w3  14 4107603335
            !r27 = gg r23 r26 r25 r24 w8  20 1163531501
            !r28 = gg r24 r27 r26 r25 w13 5  2850285829
            !r29 = gg r25 r28 r27 r26 w2  9  4243563512
            !r30 = gg r26 r29 r28 r27 w7  14 1735328473
            !r31 = gg r27 r30 r29 r28 w12 20 2368359562
            -- Round 3
            !r32 = hh r28 r31 r30 r29 w5  4  4294588738
            !r33 = hh r29 r32 r31 r30 w8  11 2272392833
            !r34 = hh r30 r33 r32 r31 w11 16 1839030562
            !r35 = hh r31 r34 r33 r32 w14 23 4259657740
            !r36 = hh r32 r35 r34 r33 w1  4  2763975236
            !r37 = hh r33 r36 r35 r34 w4  11 1272893353
            !r38 = hh r34 r37 r36 r35 w7  16 4139469664
            !r39 = hh r35 r38 r37 r36 w10 23 3200236656
            !r40 = hh r36 r39 r38 r37 w13 4  681279174
            !r41 = hh r37 r40 r39 r38 w0  11 3936430074
            !r42 = hh r38 r41 r40 r39 w3  16 3572445317
            !r43 = hh r39 r42 r41 r40 w6  23 76029189
            !r44 = hh r40 r43 r42 r41 w9  4  3654602809
            !r45 = hh r41 r44 r43 r42 w12 11 3873151461
            !r46 = hh r42 r45 r44 r43 w15 16 530742520
            !r47 = hh r43 r46 r45 r44 w2  23 3299628645
            -- Round 4
            !r48 = ii r44 r47 r46 r45 w0  6  4096336452
            !r49 = ii r45 r48 r47 r46 w7  10 1126891415
            !r50 = ii r46 r49 r48 r47 w14 15 2878612391
            !r51 = ii r47 r50 r49 r48 w5  21 4237533241
            !r52 = ii r48 r51 r50 r49 w12 6  1700485571
            !r53 = ii r49 r52 r51 r50 w3  10 2399980690
            !r54 = ii r50 r53 r52 r51 w10 15 4293915773
            !r55 = ii r51 r54 r53 r52 w1  21 2240044497
            !r56 = ii r52 r55 r54 r53 w8  6  1873313359
            !r57 = ii r53 r56 r55 r54 w15 10 4264355552
            !r58 = ii r54 r57 r56 r55 w6  15 2734768916
            !r59 = ii r55 r58 r57 r56 w13 21 1309151649
            !r60 = ii r56 r59 r58 r57 w4  6  4149444226
            !r61 = ii r57 r60 r59 r58 w11 10 3174756917
            !r62 = ii r58 r61 r60 r59 w2  15 718787259
            !r63 = ii r59 r62 r61 r60 w9  21 3951481745
        in MD5Digest r60 r63 r62 r61
        where
        f !x !y !z = (x .&. y) .|. ((complement x) .&. z)
        {-# INLINE f #-}
        g !x !y !z = (x .&. z) .|. (y .&. (complement z))
        {-# INLINE g #-}
        h !x !y !z = (x `xor` y `xor` z)
        {-# INLINE h #-}
        i !x !y !z = y `xor` (x .|. (complement z))
        {-# INLINE i #-}
        ff a b c d !x s ac = {-# SCC "ff" #-}
                let !a' = f b c d + x + ac + a
                    !a'' = rotateL a' s
                in a'' + b
        {-# INLINE ff #-}
        gg a b c d !x s ac = {-# SCC "gg" #-}
                let !a' = g b c d + x + ac + a
                    !a'' = rotateL a' s
                in a'' + b
        {-# INLINE gg #-}
        hh a b c d !x s ac = {-# SCC "hh" #-}
                let !a' = h b c d + x + ac + a
                    !a'' = rotateL a' s
                    in a'' + b
        {-# INLINE hh #-}
        ii a b c d  !x s ac = {-# SCC "ii" #-}
                let !a' = i b c d + x + ac + a
                    !a'' = rotateL a' s
                in a'' + b
        {-# INLINE ii #-}

-- | mix one Digest and 16 words by applying the md5-rounds
mixRaw ::
       MD5Digest
    -> Word32 -> Word32 -> Word32 -> Word32
    -> Word32 -> Word32 -> Word32 -> Word32
    -> Word32 -> Word32 -> Word32 -> Word32
    -> Word32 -> Word32 -> Word32 -> Word32
    -> MD5Digest
{- mix is the same as applyMD5RoundsRaw, except without INLINE pragma -}
mixRaw = applyMD5RoundsRaw

----- Some quick and dirty instances follow -----

-- todo: this is not offical
instance Show MD5Digest where
  show (MD5Digest a b c d) = "HashMD5_"++ toHex a ++ toHex b ++ toHex c ++ toHex d
    where
      toHex :: Integral a => a -> String
      toHex x = reverse $ take 8 $ hex x where
      hex x = (digits !! (fromIntegral (x `mod` 16))) : hex ( x `div` 16)
      digits = "0123456789ABCDEF"
