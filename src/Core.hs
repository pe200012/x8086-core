{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core where

import           Control.Lens
import           Data.Bits                      ( Bits(..) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Vector.Unboxed            ( (!)
                                                , (!?)
                                                , Vector
                                                , replicate
                                                )
import qualified Data.Vector.Unboxed           as V
import           Data.Word                      ( Word16
                                                , Word8
                                                )
import           Prelude                 hiding ( replicate )

data CPU = CPU
    { _ax    :: Word16
    , _bx    :: Word16
    , _cx    :: Word16
    , _dx    :: Word16
    , _sp    :: Word16
    , _bp    :: Word16
    , _si    :: Word16
    , _di    :: Word16
    , _ip    :: Word16
    , _cs    :: Word16
    , _ds    :: Word16
    , _es    :: Word16
    , _ss    :: Word16
    , _flags :: Word16
    }
    deriving (Show, Eq)

makeLenses ''CPU

al :: Lens' CPU Word8
al f c = fmap (\x -> c { _ax = (_ax c .&. 0xFF00) .|. x }) (fromIntegral <$> f (fromIntegral (_ax c)))

ah :: Lens' CPU Word8
ah f c = fmap (\x -> c { _ax = (_ax c .&. 0x00FF) .|. (x `shiftL` 8) }) (fromIntegral <$> f (fromIntegral (_ax c `shiftR` 8)))

bl :: Lens' CPU Word8
bl f c = fmap (\x -> c { _bx = (_bx c .&. 0xFF00) .|. x }) (fromIntegral <$> f (fromIntegral (_bx c)))

bh :: Lens' CPU Word8
bh f c = fmap (\x -> c { _bx = (_bx c .&. 0x00FF) .|. (x `shiftL` 8) }) (fromIntegral <$> f (fromIntegral (_bx c `shiftR` 8)))

cl :: Lens' CPU Word8
cl f c = fmap (\x -> c { _cx = (_cx c .&. 0xFF00) .|. x }) (fromIntegral <$> f (fromIntegral (_cx c)))

ch :: Lens' CPU Word8
ch f c = fmap (\x -> c { _cx = (_cx c .&. 0x00FF) .|. (x `shiftL` 8) }) (fromIntegral <$> f (fromIntegral (_cx c `shiftR` 8)))

dl :: Lens' CPU Word8
dl f c = fmap (\x -> c { _dx = (_dx c .&. 0xFF00) .|. x }) (fromIntegral <$> f (fromIntegral (_dx c)))

dh :: Lens' CPU Word8
dh f c = fmap (\x -> c { _dx = (_dx c .&. 0x00FF) .|. (x `shiftL` 8) }) (fromIntegral <$> f (fromIntegral (_dx c `shiftR` 8)))

lowBit :: Word16 -> Word8
lowBit = fromIntegral

highBit :: Word16 -> Word8
highBit = fromIntegral . (`shiftR` 8)

{-

>>> sampleCPU = CPU (5 + 10 * 2 ^ 8) 0 0 0 0 0 0 0 0 0 0 0 0 0

>>> sampleCPU
CPU {_ax = 2565, _bx = 0, _cx = 0, _dx = 0, _sp = 0, _bp = 0, _si = 0, _di = 0, _ip = 0, _cs = 0, _ds = 0, _es = 0, _ss = 0, _flags = 0}

>>> sampleCPU ^. ah
10

>>> sampleCPU ^. al
5

-}

newtype Memory = Memory { unMemory :: Vector Word8 }
    deriving (Show, Eq)

type instance Index Memory = Word16
type instance IxValue Memory = Word8

instance At Memory where
    at i = lens ((!? fromIntegral i) . unMemory) (\(Memory m) x -> Memory (m & ix (fromIntegral i) .~ fromMaybe 0 x))
instance Ixed Memory

-- >>> (Memory $ replicate 10 0xFF) ^? ix 5

memoryUnits :: IndexedTraversal' Word Memory Word8
memoryUnits f (Memory m) = Memory . V.fromList <$> traverse go is
  where
    go (n :: Int) = indexed f (fromIntegral n :: Word) (m ! n)
    is = [0 .. V.length m - 1]

-- >>> toListOf memoryUnits $ Memory (replicate 10 0xFF)
-- [255,255,255,255,255,255,255,255,255,255]

maxMemory :: Int
maxMemory = 2 ^ (20 :: Int)
{-# INLINE maxMemory #-}

emptyMemory :: Memory
emptyMemory = Memory $ replicate maxMemory 0
{-# INLINE emptyMemory #-}

physicalAddr :: Word16 -> Word16 -> Word16
physicalAddr segment offset = segment * 16 + offset
{-# INLINE physicalAddr #-}

peekMemory :: Memory -> Word16 -> Maybe Word8
peekMemory (Memory mem) addr | addr < 0                       = Nothing
                             | addr >= fromIntegral maxMemory = Nothing
                             | otherwise                      = Just (mem ! fromIntegral addr)
{-# INLINE peekMemory #-}

data Computer = Computer
    { _cpu    :: CPU
    , _memory :: Memory
    }
    deriving (Show, Eq)

makeLenses ''Computer

data Register = AX | BX | CX | DX | AH | BH | CH | DH | AL | BL | CL | DL | SP | BP | SI | DI | IP | CS | DS | ES | SS | FLAGS
    deriving (Show, Eq, Enum)

registerSelector :: (Functor f1, Functor f2) => Register -> Either ((Word8 -> f2 Word8) -> CPU -> f2 CPU) ((Word16 -> f1 Word16) -> CPU -> f1 CPU)
registerSelector AX    = Right ax
registerSelector BX    = Right bx
registerSelector CX    = Right cx
registerSelector DX    = Right dx
registerSelector AH    = Left ah
registerSelector BH    = Left bh
registerSelector CH    = Left ch
registerSelector DH    = Left dh
registerSelector AL    = Left al
registerSelector BL    = Left bl
registerSelector CL    = Left cl
registerSelector DL    = Left dl
registerSelector SP    = Right sp
registerSelector BP    = Right bp
registerSelector SI    = Right si
registerSelector DI    = Right di
registerSelector IP    = Right ip
registerSelector CS    = Right cs
registerSelector DS    = Right ds
registerSelector ES    = Right es
registerSelector SS    = Right ss
registerSelector FLAGS = Right flags
{-# INLINE registerSelector #-}

compareRegister :: Register -> Register -> Ordering
compareRegister AL AL = EQ
compareRegister AL BL = EQ
compareRegister AL CL = EQ
compareRegister AL DL = EQ
compareRegister AL AH = EQ
compareRegister AL BH = EQ
compareRegister AL CH = EQ
compareRegister AL DH = EQ
compareRegister AL _  = LT
compareRegister AH AL = EQ
compareRegister AH BL = EQ
compareRegister AH CL = EQ
compareRegister AH DL = EQ
compareRegister AH AH = EQ
compareRegister AH BH = EQ
compareRegister AH CH = EQ
compareRegister AH DH = EQ
compareRegister AH _  = LT
compareRegister BL AL = EQ
compareRegister BL BL = EQ
compareRegister BL CL = EQ
compareRegister BL DL = EQ
compareRegister BL AH = EQ
compareRegister BL BH = EQ
compareRegister BL CH = EQ
compareRegister BL DH = EQ
compareRegister BL _  = LT
compareRegister BH AL = EQ
compareRegister BH BL = EQ
compareRegister BH CL = EQ
compareRegister BH DL = EQ
compareRegister BH AH = EQ
compareRegister BH BH = EQ
compareRegister BH CH = EQ
compareRegister BH DH = EQ
compareRegister BH _  = LT
compareRegister CL AL = EQ
compareRegister CL BL = EQ
compareRegister CL CL = EQ
compareRegister CL DL = EQ
compareRegister CL AH = EQ
compareRegister CL BH = EQ
compareRegister CL CH = EQ
compareRegister CL DH = EQ
compareRegister CL _  = LT
compareRegister CH AL = EQ
compareRegister CH BL = EQ
compareRegister CH CL = EQ
compareRegister CH DL = EQ
compareRegister CH AH = EQ
compareRegister CH BH = EQ
compareRegister CH CH = EQ
compareRegister CH DH = EQ
compareRegister CH _  = LT
compareRegister DL AL = EQ
compareRegister DL BL = EQ
compareRegister DL CL = EQ
compareRegister DL DL = EQ
compareRegister DL AH = EQ
compareRegister DL BH = EQ
compareRegister DL CH = EQ
compareRegister DL DH = EQ
compareRegister DL _  = LT
compareRegister DH AL = EQ
compareRegister DH BL = EQ
compareRegister DH CL = EQ
compareRegister DH DL = EQ
compareRegister DH AH = EQ
compareRegister DH BH = EQ
compareRegister DH CH = EQ
compareRegister DH DH = EQ
compareRegister DH _  = LT
compareRegister _  _  = EQ

data Operand = Register Register | Immediate16 Word16 | Immediate8 Word8 | MemoryUnit Word16
    deriving (Show, Eq)

mov :: Operand -> Operand -> Computer -> Either String Computer
mov (Register r1) (Register r2) c = case compareRegister r1 r2 of
    LT -> Left "mov: register size mismatch"
    _ ->
        let (Right f1) = registerSelector @Identity @Identity r1
            (Right f2) = registerSelector @(Const Word16) @Identity r2
        in  Right $ c & cpu . f1 .~ (c ^. cpu . f2)
mov (Register r1) (Immediate16 i) c = case compareRegister r1 AX of
    LT -> Left "mov: register size mismatch"
    _  -> let (Right f1) = registerSelector @Identity @Identity r1 in Right $ c & cpu . f1 .~ i
mov (Register r1) (Immediate8 i) c = case compareRegister AL r1 of
    LT -> Left "mov: register size mismatch"
    _  -> let (Right f1) = registerSelector @Identity @Identity r1 in Right $ c & cpu . f1 .~ fromIntegral i
mov (Register r1) (MemoryUnit m) c = case compareRegister r1 AX of
    LT -> Left "mov: register size mismatch"
    _  -> case c ^? memory . ix m of
        Nothing -> Left "mov: memory unit out of bounds"
        Just i  -> let (Right f1) = registerSelector @Identity @Identity r1 in Right $ c & cpu . f1 .~ fromIntegral i
mov (Immediate16 _) _ _ = Left "mov: immediate16 cannot be used as destination"
mov (Immediate8  _) _ _ = Left "mov: immediate8 cannot be used as destination"
mov (MemoryUnit u) (Register r) c
    | u < 0 || fromIntegral u >= memoryBound = Left "mov: memory unit out of bounds"
    | otherwise = case compareRegister r AX of
        LT -> let (Left f1) = registerSelector @(Const Word16) @(Const Word8) r in Right $ c & memory . ix u .~ (c ^. cpu . f1)
        _
            | fromIntegral (u + 1) > memoryBound
            -> Left "mov: memory unit out of bounds"
            | otherwise
            -> let (Right f1) = registerSelector @(Const Word8) @Identity r
               in  Right $ c & memory . ix u .~ (c ^. cpu . f1 . to lowBit) & memory . ix (u + 1) .~ (c ^. cpu . f1 . to highBit)
    where memoryBound = c ^. memory . to (V.length . unMemory)
mov (MemoryUnit u) (Immediate16 i) c | u < 0 || fromIntegral u >= memoryBound - 1 = Left "mov: memory unit out of bounds"
                                     | otherwise = Right $ c & memory . ix u .~ lowBit i & memory . ix (u + 1) .~ highBit i
    where memoryBound = c ^. memory . to (V.length . unMemory)
mov (MemoryUnit u) (Immediate8 i) c | u < 0 || fromIntegral u >= memoryBound = Left "mov: memory unit out of bounds"
                                    | otherwise                              = Right $ c & memory . ix u .~ i
    where memoryBound = c ^. memory . to (V.length . unMemory)
mov (MemoryUnit _) (MemoryUnit _) _ = Left "mov: memory unit cannot be used both as source and destination"
