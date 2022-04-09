{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Core
    ( CPU(..)
    , emptyCPU
    , al
    , ah
    , bl
    , bh
    , cl
    , ch
    , dl
    , dh
    , lowBit
    , highBit
    , Memory(unMemory)
    , memoryUnits
    , maxMemory
    , emptyMemory
    , physicalAddr
    , peekMemory
    , VideoMode(..)
    , videoModeCode
    , Computer(..)
    , mov
    , xchg
    , Operand(..)
    , int
    ) where

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

emptyCPU :: CPU
emptyCPU = CPU 0 0 0 0 0 0 0 0 0 0 0 0 0 0

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

data VideoMode = TextMode
               { _textModeWidth  :: Int
               , _textModeHeight :: Int
               , _textModeColors :: Int
               , _textModePages :: Int
               }
               | GraphicMode
               { _graphicModeWidth  :: Int
               , _graphicModeHeight :: Int
               , _graphicModePixelWidth  :: Int
               , _graphicModePixelHeight :: Int
               , _graphicModeColors :: Int
               , _graphicModePages :: Int
               }
                deriving (Show, Eq)

videoModeCode :: Word8 -> Either String VideoMode
videoModeCode 0x00 = Right $ TextMode 40 25 16 8
videoModeCode 0x03 = Right $ TextMode 80 25 16 8
videoModeCode 0x13 = Right $ GraphicMode 40 25 320 200 256 1
videoModeCode _    = Left "videoModeCode: unsupported video mode"

data Computer = Computer
    { _cpu       :: CPU
    , _memory    :: Memory
    , _videoMode :: VideoMode
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
compareRegister AL    AL = EQ
compareRegister AL    BL = EQ
compareRegister AL    CL = EQ
compareRegister AL    DL = EQ
compareRegister AL    AH = EQ
compareRegister AL    BH = EQ
compareRegister AL    CH = EQ
compareRegister AL    DH = EQ
compareRegister AL    _  = LT
compareRegister AH    x  = compareRegister AL x
compareRegister BL    x  = compareRegister AL x
compareRegister BH    x  = compareRegister AL x
compareRegister CL    x  = compareRegister AL x
compareRegister CH    x  = compareRegister AL x
compareRegister DL    x  = compareRegister AL x
compareRegister DH    x  = compareRegister AL x
compareRegister AX    AL = GT
compareRegister AX    AH = GT
compareRegister AX    BL = GT
compareRegister AX    BH = GT
compareRegister AX    CL = GT
compareRegister AX    CH = GT
compareRegister AX    DL = GT
compareRegister AX    DH = GT
compareRegister AX    _  = EQ
compareRegister BX    x  = compareRegister AX x
compareRegister CX    x  = compareRegister AX x
compareRegister DX    x  = compareRegister AX x
compareRegister SP    x  = compareRegister AX x
compareRegister BP    x  = compareRegister AX x
compareRegister SI    x  = compareRegister AX x
compareRegister DI    x  = compareRegister AX x
compareRegister IP    x  = compareRegister AX x
compareRegister CS    x  = compareRegister AX x
compareRegister DS    x  = compareRegister AX x
compareRegister ES    x  = compareRegister AX x
compareRegister SS    x  = compareRegister AX x
compareRegister FLAGS x  = compareRegister AX x

isSegmentRegister :: Register -> Bool
isSegmentRegister CS = True
isSegmentRegister DS = True
isSegmentRegister ES = True
isSegmentRegister SS = True
isSegmentRegister _  = False

data Operand = Register Register | Immediate16 Word16 | Immediate8 Word8 | MemoryUnit Word16
    deriving (Show, Eq)

-- | @mov dst src c@ copies the value of `src` to `dst`.
mov :: Operand -> Operand -> Computer -> Either String Computer
mov (Register r1) (Register r2) c
    | r1 == IP = Left "mov: cannot change IP register"
    | otherwise = case compareRegister r1 r2 of
        LT -> Left "mov: register size mismatch"
        GT -> Left "mov: register size mismatch"
        EQ ->
            let (Right f1) = registerSelector @Identity @Identity r1
                (Right f2) = registerSelector @(Const Word16) @Identity r2
            in  Right $ c & cpu . f1 .~ (c ^. cpu . f2)
mov (Register r1) (Immediate16 i) c
    | isSegmentRegister r1 = Left "mov: cannot directly change segment register"
    | r1 == IP = Left "mov: cannot change IP register"
    | otherwise = case compareRegister r1 AX of
        LT -> Left "mov: register size mismatch"
        _  -> let (Right f1) = registerSelector @Identity @Identity r1 in Right $ c & cpu . f1 .~ i
mov (Register r1) (Immediate8 i) c
    | isSegmentRegister r1 = Left "mov: cannot directly change segment register"
    | r1 == IP = Left "mov: cannot change IP register"
    | otherwise = case compareRegister AL r1 of
        LT -> Left "mov: register size mismatch"
        _  -> let (Left f1) = registerSelector @Identity @Identity r1 in Right $ c & cpu . f1 .~ fromIntegral i
mov (Register r1) (MemoryUnit m) c
    | isSegmentRegister r1 = Left "mov: cannot directly change segment register"
    | r1 == IP = Left "mov: cannot change IP register"
    | otherwise = case compareRegister r1 AX of
        LT -> case c ^? memory . ix m of
            Nothing -> Left "mov: invalid memory address"
            Just v  -> let (Left f1) = registerSelector @(Const Word8) @Identity r1 in Right $ c & cpu . f1 .~ v
        _ -> case c ^? memory . ix m of
            Nothing                  -> Left "mov: memory unit out of bounds"
            Just (fromIntegral -> i) -> case c ^? memory . ix (m + 1) of
                Nothing                  -> Left "mov: memory unit out of bounds"
                Just (fromIntegral -> j) -> let (Right f1) = registerSelector @Identity @Identity r1 in Right $ c & cpu . f1 .~ (i .|. (j `shiftL` 8))
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

{-

>>> c = emptyCPU & ax .~ 0
>>> m = Memory $ V.fromList [0,0,0,0]
>>> com = Computer c m

>>> (_ax._cpu <$> mov (Register AL) (Immediate8 0x12) com) == Right 0x0012
True

>>> (_ax._cpu <$> mov (Register AX) (Immediate16 0x2424) com) == Right 0x2424
True

>>> (_memory <$> mov (MemoryUnit 1) (Immediate16 0x2421) com)
Right (Memory {unMemory = [0,33,36,0]})

>>> (_memory <$> mov (MemoryUnit 3) (Immediate16 0x2421) com)
Left "mov: memory unit out of bounds"

>>> Right com1 = mov (MemoryUnit 2) (Immediate16 0x1981) com

>>> mov (Register AX) (MemoryUnit 2) com1
Right (Computer {_cpu = CPU {_ax = 6529, _bx = 0, _cx = 0, _dx = 0, _sp = 0, _bp = 0, _si = 0, _di = 0, _ip = 0, _cs = 0, _ds = 0, _es = 0, _ss = 0, _flags = 0}, _memory = Memory {unMemory = [0,0,129,25]}})

>>> mov (Register AL) (MemoryUnit 2) com1
Right (Computer {_cpu = CPU {_ax = 129, _bx = 0, _cx = 0, _dx = 0, _sp = 0, _bp = 0, _si = 0, _di = 0, _ip = 0, _cs = 0, _ds = 0, _es = 0, _ss = 0, _flags = 0}, _memory = Memory {unMemory = [0,0,129,25]}})

-}

-- |
xchg :: Operand -> Operand -> Computer -> Either String Computer
xchg (Register r1) (Register r2) c = case compareRegister r1 r2 of
    LT -> Left "xchg: register size mismatch"
    GT -> Left "xchg: register size mismatch"
    EQ ->
        let (Right rr1) = registerSelector @Identity @Identity r1
            (Right rr2) = registerSelector @Identity @Identity r2
            (Right wr1) = registerSelector @(Const Word16) @Identity r1
            (Right wr2) = registerSelector @(Const Word16) @Identity r2
        in  Right $ c & cpu . rr1 .~ (c ^. cpu . wr2) & cpu . rr2 .~ (c ^. cpu . wr1)
xchg (Register r) (MemoryUnit u) c = case compareRegister r AX of
    LT ->
        let (Left rr) = registerSelector @(Const Word16) @(Const Word8) r
            (Left wr) = registerSelector @Identity @Identity r
        in  case c ^? memory . ix u of
                Nothing -> Left "xchg: memory unit out of bounds"
                Just i  -> Right $ c & cpu . wr .~ i & memory . ix u .~ (c ^. cpu . rr)
    _ -> case c ^? memory . ix u of
        Nothing                  -> Left "xchg: memory unit out of bounds"
        Just (fromIntegral -> i) -> case c ^? memory . ix (u + 1) of
            Nothing -> Left "xchg: memory unit out of bounds"
            Just (fromIntegral -> j) ->
                let val        = i .|. (j `shiftL` 8)
                    (Right wr) = registerSelector @Identity @Identity r
                    (Right rr) = registerSelector @(Const Word16) @Identity r
                in  Right $ c & cpu . wr .~ val & memory . ix u .~ lowBit (c ^. cpu . rr) & memory . ix (u + 1) .~ highBit (c ^. cpu . rr)
xchg (Register    _) (Immediate16 _) _ = Left "xchg: immediate16 cannot be used as destination"
xchg (Register    _) (Immediate8  _) _ = Left "xchg: immediate8 cannot be used as destination"
xchg (Immediate16 _) _               _ = Left "xchg: immediate16 cannot be used as source"
xchg (Immediate8  _) _               _ = Left "xchg: immediate8 cannot be used as source"
-- xchg (MemoryUnit u) (Register r) c = case
xchg (MemoryUnit  _) (MemoryUnit _)  _ = Left "xchg: memory unit cannot be used both as source and destination"
xchg (MemoryUnit  u) (Register   r)  c = case compareRegister r AX of
    LT ->
        let (Left rr) = registerSelector @(Const Word16) @(Const Word8) r
            (Left wr) = registerSelector @Identity @Identity r
        in  case c ^? memory . ix u of
                Nothing -> Left "xchg: memory unit out of bounds"
                Just i  -> Right $ c & cpu . wr .~ i & memory . ix u .~ (c ^. cpu . rr)
    _ -> case c ^? memory . ix u of
        Nothing                  -> Left "xchg: memory unit out of bounds"
        Just (fromIntegral -> i) -> case c ^? memory . ix (u + 1) of
            Nothing -> Left "xchg: memory unit out of bounds"
            Just (fromIntegral -> j) ->
                let val        = i .|. (j `shiftL` 8)
                    (Right wr) = registerSelector @Identity @Identity r
                    (Right rr) = registerSelector @(Const Word16) @Identity r
                in  Right $ c & cpu . wr .~ val & memory . ix u .~ lowBit (c ^. cpu . rr) & memory . ix (u + 1) .~ highBit (c ^. cpu . rr)
xchg (MemoryUnit _) (Immediate16 _) _ = Left "xchg: immediate16 cannot be used as destination"
xchg (MemoryUnit _) (Immediate8  _) _ = Left "xchg: immediate8 cannot be used as destination"

{-

>>> c = emptyCPU & ax .~ 0x0012 & bx .~ 0x1211
>>> m = Memory $ V.fromList [0x0011,0,0,0]
>>> com = Computer c m

>>> xchg (Register AL) (MemoryUnit 0) com
Right (Computer {_cpu = CPU {_ax = 17, _bx = 4625, _cx = 0, _dx = 0, _sp = 0, _bp = 0, _si = 0, _di = 0, _ip = 0, _cs = 0, _ds = 0, _es = 0, _ss = 0, _flags = 0}, _memory = Memory {unMemory = [18,0,0,0]}})

>>> xchg (Register AX) (Register BX) com
Right (Computer {_cpu = CPU {_ax = 4625, _bx = 18, _cx = 0, _dx = 0, _sp = 0, _bp = 0, _si = 0, _di = 0, _ip = 0, _cs = 0, _ds = 0, _es = 0, _ss = 0, _flags = 0}, _memory = Memory {unMemory = [17,0,0,0]}})

-}

-- | 8086 interrupts
int :: Int -> Computer -> Either String Computer
int _ _ = Left "int: unsupported interrupt"
