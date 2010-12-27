{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- This C++ code http://siyobik.info/index.php?module=pastebin&id=543
-- got linked in IRC, alongside a naive Haskell port. 
--
-- The code is random and doesn't do anything; however it has many
-- interesting properties hidden by the C++ implementation.
--
-- Converting it to a high level Haskell representation explores those
-- properties in great detail, laying them bare. It is a classic example
-- of low level code being harder to optimize/understand than high level
-- code.
--
-- What at first glance looks like a trivial fold, at second glance like
-- a random-access hash, and at third glance a Turing machine... Is
-- actually none of them, yet contains elements from all.
--
-- Take note. This is Haskell!
import Data.Vector as V
import Data.Word
import Data.Bits


determineVKey :: Vector Word32 -> Word32
determineVKey = compute . decode


-- | Only part of the input is used to hash the key, the rest is control
-- data. May as well show it like it is:
data Instr = Seed Word32 | Mutate Word32 
           | JmpFwd Word16 | JmpRev Word16 
           | JmpMut Word16
        deriving(Eq,Show)

-- | Pretty explicit about the various kinds of inputs... Compare to the
-- if + switch in C version.
decode :: Vector Word32 -> Vector Instr
decode = V.imap op where
    op 0 n = Seed n
    op _ n | testBit n 32 = Mutate n
           | otherwise = let code = low $ low n
                         in jmp code (high n)
    jmp :: Word8 -> (Word16 -> Instr)
    jmp 0 = JmpFwd
    jmp 1 = JmpRev
    jmp 3 = JmpMut
    jmp _ = error "bad jump opcode"
    
-- | The Instr type gives a pretty good hint of what's going on, this
-- confirms it.
compute :: Vector Instr -> Word32
compute = run 0 0 where
    run key pos vec = case vec V.!? pos of
        Nothing -> key -- terminal condition (out of bounds lol)
        Just instr -> let (dx,key') = eval key instr
                      in run key' (pos+coerce dx) vec
    eval :: Word32 -> Instr -> (Word16,Word32)
    eval _   (Seed n)   = (1, n)
    eval key (Mutate n) = (1, key + n)
    eval key (JmpFwd j) = (j, key)
    eval key (JmpRev j) = ((-j), key)
    eval key (JmpMut n) = ( 0 |+ high (high key)
                          , key `xor` (0 |+ n)
                          )


-- | Adjacent word size (8,16,32,64) conversions.
--
-- Not strictly necessary, but makes the above code a tad nicer. Note
-- the use of a functional dependency, to hint the type checker that
-- type b can always be determined from just a.
class (Bits a, Bits b) => Composite a b | a->b where
    low :: a -> b   -- ^ lower half
    high :: a -> b  -- ^ upper half
    (|+) :: b -> b -> a -- ^ 

coerce :: (Integral a, Num b) => a -> b
coerce = fromIntegral

instance Composite Word32 Word16 where
    low = coerce . (.&. 0xFFFF)
    high = low . (`shiftR` 16)
    a |+ b = shiftL 16 (coerce a) .|. coerce b

instance Composite Word16 Word8 where
    low = coerce . (.&. 0xFF)
    high = low . (`shiftR` 8)
    a |+ b = shiftL 8 (coerce a) .|. coerce b 
