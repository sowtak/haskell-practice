{-# LANGUAGE
  ScopedTypeVariables, BangPatterns, TupleSections, ExplicitForAll,
  LambdaCase, MultiWayIf, Unsafe, RecordWildCards, FlexibleContexts, CPP,
  NoMonomorphismRestriction, GADTs, PatternGuards, MagicHash,
  UnboxedTuples,
  RankNTypes, EmptyDataDecls, EmptyCase, ViewPatterns, PolyKinds,
  TypeFamilies, OverloadedStrings, FlexibleInstances, UndecidableInstances,
  DefaultSignatures, GeneralizedNewtypeDeriving, StandaloneDeriving,
  DeriveGeneric, DeriveFunctor, DeriveDataTypeable, DeriveFoldable,
  DeriveTraversable, DeriveDataTypeable, FlexibleInstances,
  MultiParamTypeClasses, TypeApplications, RecordWildCards,
  PackageImports,
  NumericUnderscores, ConstraintKinds #-}
{-# OPTIONS_GHC -O2 #-}

#define PHASE_FUSED [1]
#define PHASE_INNER [0]
#define INLINE_FUSED INLINE PHASE_FUSED
#define INLINE_INNER INLINE PHASE_INNER

import Prelude
import Data.Bits
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Ord
import Data.Int
import Data.Word
import Data.Char
import Data.Ratio
import Data.Function
import Data.STRef
import Data.IORef
import Data.Monoid
import Data.Functor
import Data.Functor.Identity
import Data.Data
import Data.Typeable
import GHC.Generics
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Control.Monad.ST
import Control.Monad.ST.Lazy (strictToLazyST, lazyToStrictST)
import qualified Control.Monad.ST.Lazy as STL
-- import Control.Monad.ST.Safe
import Control.DeepSeq
import Data.Coerce
import qualified Data.ByteString as BSW
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSLW
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Internal as BSU
import qualified Data.IntPSQ as IntPSQ
import Data.IntPSQ (IntPSQ)
import qualified Data.OrdPSQ as OrdPSQ
import Data.OrdPSQ (OrdPSQ)
import qualified Data.HashPSQ as HashPSQ
import Data.HashPSQ (HashPSQ)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashMap.Lazy as HashMapL
import qualified Data.HashMap.Lazy as HML
import Data.HashMap.Strict (HashMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntMap.Strict as IMS
import qualified Data.IntMap.Lazy as IntMapL
import qualified Data.IntMap.Lazy as IML
import Data.IntMap (IntMap)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as MapL
import Data.Map.Strict (Map)
import Data.List as List
import Data.Hashable (Hashable)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Array.IArray as A
import qualified Data.Array.MArray.Safe as A
import qualified Data.Array.MArray as A
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray (IArray)
import Data.Array.MArray.Safe (MArray)
import Data.Array.IO.Safe (IOArray, IOUArray)
import Data.Array.ST.Safe (STArray, STUArray, runSTArray, runSTUArray)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as VPM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Generic.New as VGN
import qualified Data.Vector.Fusion.Bundle.Monadic as VFBM
import qualified Data.Vector.Fusion.Bundle as VFB
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Fusion.Bundle.Size as VFBS
import qualified Data.Vector.Fusion.Util as VFU
import qualified Data.Vector.Algorithms.Intro as VAIT
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Debug.Trace
import Unsafe.Coerce
import Foreign.ForeignPtr
import GHC.Float (castWord64ToDouble, castDoubleToWord64)
import GHC.Exts (build, Int(..), Int#,
                 (+#), (*#), (-#), (<#), (>=#), (==#), quotRemInt#,
                 remInt#, uncheckedIShiftRL#, andI#, orI#,
                isTrue#, Addr#, Ptr(..))

main :: IO ()
main = do
  x <- readLn @Double
  print $ sqrt $ x * (12800000+x)
  return ()

debug :: Bool
debug = True

#define DefDebugAux(fct,ty,debugvar,dbg,rel)\
  fct :: ty; {-# INLINE fct #-}; fct | debugvar = dbg | otherwise = rel
#define DefDebug(fct,ty,rel) DefDebugAux(fct,ty,debug,Debug.Trace.fct,rel)
#define DefDebugC(fct,ty,rel) DefDebug(fct,ty,const (rel))

DefDebugC(trace, String -> a -> a, id)
DefDebug(traceId, String -> String, id)
DefDebugC(traceShow, Show b => b -> a -> a, id)
DefDebug(traceShowId, Show a => a -> a, id)
DefDebugC(traceStack, String -> a -> a, id)
DefDebugC(traceIO, String -> IO (), return ())
DefDebugC(traceM, Applicative f => String -> f (), pure ())
DefDebugC(traceShowM, (Show a, Applicative f) => a -> f (), pure ())
DefDebugC(traceEvent, String -> a -> a, id)
DefDebugC(traceEventIO, String -> IO (), pure ())
DefDebugC(traceMarker, String -> a -> a, id)
DefDebugC(traceMarkerIO, String -> IO (), pure ())

#undef DefDebugAux
#undef DefDebug
#undef DefDebugC

traceShowIO :: Show a => a -> IO ()
{-# INLINE traceShowIO #-}
traceShowIO | debug     = Debug.Trace.traceIO . show
            | otherwise = const $ pure ()

#define IL(f) {-# INLINE f #-}; f

IL(putBuilder) = BSB.hPutBuilder stdout

printVecInLines, printVecInSpcSepLn ::
  (VG.Vector v a, ShowAsBuilder a) => v a -> IO ()
IL(printVecInLines) = putBuilder . v2BLines
IL(printVecInSpcSepLn) = putBuilder . v2BSpcSepLn

class ShowAsBuilder a where
  showAsBuilder :: a -> BSB.Builder
  default showAsBuilder :: (Show a) => a -> BSB.Builder
  IL(showAsBuilder) = BSB.string8 . show

-- Inconsistent with show
instance {-# INCOHERENT #-} (ShowAsBuilder a, VG.Vector v a) => ShowAsBuilder (v a) where
  IL(showAsBuilder) = v2BSpcSep

#define INS(t,f) instance ShowAsBuilder t where { IL(showAsBuilder)=f }
INS(Int,BSB.intDec)
INS(Int8,BSB.int8Dec)
INS(Int16,BSB.int16Dec)
INS(Int32,BSB.int32Dec)
INS(Int64,BSB.int64Dec)
INS(Word,BSB.wordDec)
INS(Word8,BSB.word8Dec)
INS(Word16,BSB.word16Dec)
INS(Word32,BSB.word32Dec)
INS(Word64,BSB.word64Dec)
INS(Integer,BSB.integerDec)
INS(Float,BSB.floatDec)
INS(Double,BSB.doubleDec)
-- INS(String,BSB.string8) -- Inconsistent with Show
-- INS(BS.ByteString,BSB.byteString) -- Inconsistent with Show
-- INS(BSL.ByteString,BSB.lazyByteString) -- Inconsisitent with Show
#undef INS

-- Inconsistent with Show
instance (ShowAsBuilder a, ShowAsBuilder b) => ShowAsBuilder (a,b) where
  IL(showAsBuilder) = showTupAsBuilder
instance (ShowAsBuilder a, ShowAsBuilder b, ShowAsBuilder c) =>
  ShowAsBuilder (a,b,c) where
  IL(showAsBuilder) = showTup3AsBuilder
instance (ShowAsBuilder a, ShowAsBuilder b, ShowAsBuilder c, ShowAsBuilder d) =>
  ShowAsBuilder (a,b,c,d) where
  IL(showAsBuilder) = showTup4AsBuilder

IL(showTupAsBuilderWith)
  :: (a -> BSB.Builder) -> (b -> BSB.Builder) -> (a,b) -> BSB.Builder
showTupAsBuilderWith showA showB
  = \(a,b) -> (showA a <>) $ BSB.char7 ' ' <> showB b
IL(showTupAsBuilder) :: (ShowAsBuilder a, ShowAsBuilder b)
  => (a,b) -> BSB.Builder
showTupAsBuilder = showTupAsBuilderWith showAsBuilder showAsBuilder 

IL(showTup3AsBuilderWith) :: (a -> BSB.Builder) -> (b -> BSB.Builder) ->
  (c -> BSB.Builder) -> (a,b,c) -> BSB.Builder
showTup3AsBuilderWith showA showB showC
  = \(a,b,c) -> (showA a <>) $ (BSB.char7 ' ' <>) $ (showB b <>)
                $ (BSB.char7 ' ' <>) $ showC c
IL(showTup3AsBuilder) :: (ShowAsBuilder a, ShowAsBuilder b, ShowAsBuilder c)
  => (a,b,c) -> BSB.Builder
showTup3AsBuilder
  = showTup3AsBuilderWith showAsBuilder showAsBuilder showAsBuilder

IL(showTup4AsBuilderWith) :: (a -> BSB.Builder) -> (b -> BSB.Builder) ->
  (c -> BSB.Builder) -> (d -> BSB.Builder) -> (a,b,c,d) -> BSB.Builder
showTup4AsBuilderWith showA showB showC showD
  = \(a,b,c,d) -> (showA a <>) $ (BSB.char7 ' ' <>)
                  $ showTup3AsBuilderWith showB showC showD (b,c,d)
IL(showTup4AsBuilder) ::
  (ShowAsBuilder a, ShowAsBuilder b, ShowAsBuilder c, ShowAsBuilder d) =>
  (a,b,c,d) -> BSB.Builder
showTup4AsBuilder = showTup4AsBuilderWith showAsBuilder showAsBuilder
                    showAsBuilder showAsBuilder

v2BSpcSepLn, v2BSpcSep, v2BConcat, v2BLines ::
  (VG.Vector v a, ShowAsBuilder a)
  => v a -> BSB.Builder
IL(v2BSpcSepLn) = v2BSpcSepLnWith showAsBuilder
IL(v2BSpcSep) = v2BSpcSepWith showAsBuilder
IL(v2BConcat) = v2BConcatWith showAsBuilder
IL(v2BLines) = v2BLinesWith showAsBuilder


v2BSpcSepLnWith, v2BSpcSepWith, v2BConcatWith, v2BLinesWith ::
  (VG.Vector v a)
  => (a -> BSB.Builder) -- ^ show function
  -> v a -> BSB.Builder
IL(v2BSpcSepLnWith) = v2BSpcSepPostfWith $ BS.singleton '\n'
IL(v2BSpcSepWith) = v2BSpcSepPostfWith BS.empty
IL(v2BConcatWith) showFct = VG.foldr ((<>) . showFct) mempty
IL(v2BLinesWith) showFct
  = VG.foldr (\ a -> (showFct a <>) . (BSB.char7 '\n' <>)) mempty


v2BSpcSepPostf :: (VG.Vector v a, ShowAsBuilder a)
  => BS.ByteString -- ^ postfix
  -> v a -> BSB.Builder
IL(v2BSpcSepPostf) = (`v2BSpcSepPostfWith` showAsBuilder)

v2BSpcSepPostfWith :: (VG.Vector v a)
  => BS.ByteString -- ^ postfix
  -> (a -> BSB.Builder) -- ^ show function
  -> v a -> BSB.Builder
IL(v2BSpcSepPostfWith) = vecToBuilder BS.empty $ BS.singleton ' '

IL(vecToBuilder) :: (VG.Vector v a)
  => BS.ByteString -- ^ prefix
  -> BS.ByteString -- ^ separator
  -> BS.ByteString -- ^ postfix
  -> (a -> BSB.Builder) -- ^ show function
  -> v a -> BSB.Builder
vecToBuilder !prefix !separator !postfix
  = vecToBuilder_ (BSB.byteString prefix)
                  (BSB.byteString separator)
                  (BSB.byteString postfix)


IL(vecToBuilder_) :: (VG.Vector v a)
  => BSB.Builder -- ^ prefix
  -> BSB.Builder -- ^ separator
  -> BSB.Builder -- ^ postfix
  -> (a -> BSB.Builder) -- ^ show function
  -> v a -> BSB.Builder
vecToBuilder_ !prefix !separator !postfix showFct = \vec -> prefix <>
  VG.foldr
  (\ a rest !prefx -> prefx <> (showFct a <> rest separator))
  (const postfix) vec mempty

IL(evalVals) :: [a] -> [a]
evalVals xs = build $ \c n -> foldr (c $!) n xs
IL(forceVals) :: (NFData a) => [a] -> [a]
forceVals xs = build $ \c n -> foldr (c $!!) n xs

IL(readLnWith) :: StateT BS.ByteString Maybe a -> IO a
readLnWith parser = fromJust . evalStateT parser <$> BS.getLine
IL(readContentWith) :: StateT BSL.ByteString Maybe a -> IO a
readContentWith parser = fromJust . evalStateT parser <$> BSL.getContents

IL(getVecGLn) :: (VG.Vector v a) =>
  Int -> StateT BS.ByteString Maybe a -> IO (v a)
getVecGLn n s = VG.unfoldrN n (runStateT s) <$> BS.getLine
IL(getVecGRest) :: (VG.Vector v a) =>
  Int -> StateT BSL.ByteString Maybe a -> IO (v a)
getVecGRest n s = VG.unfoldrN n (runStateT s) <$> BSL.getContents
IL(getVecLn) :: Int -> StateT BS.ByteString Maybe a -> IO (V.Vector a)
getVecLn = getVecGLn
IL(getVecRest) :: Int -> StateT BSL.ByteString Maybe a -> IO (V.Vector a)
getVecRest = getVecGRest
IL(getVecULn) :: (VU.Unbox a) =>
  Int -> StateT BS.ByteString Maybe a -> IO (VU.Vector a)
getVecULn = getVecGLn
IL(getVecURest) :: (VU.Unbox a) =>
  Int -> StateT BSL.ByteString Maybe a -> IO (VU.Vector a)
getVecURest = getVecGRest

IL(ord8) :: Char -> Word8
ord8 = fromIntegral . fromEnum
IL(chr8) :: Word8 -> Char
chr8 = toEnum . fromIntegral

IL(rIntL) :: StateT BSL.ByteString Maybe Int
rIntL = skipSpecialL $ StateT BSL.readInt
IL(rIntS) :: StateT BS.ByteString Maybe Int
rIntS = skipSpecialS $ StateT BS.readInt
IL(rStrL) :: (MonadState BSL.ByteString m) => m BS.ByteString
rStrL = skipSpecialL $ BSL.toStrict <$> state (BSLW.span (>= ord8 '!'))
IL(rStrS) :: (MonadState BS.ByteString m) => m BS.ByteString
rStrS = skipSpecialS $ state $ BSW.span (>= ord8 '!')
IL(rCharL) :: StateT BSL.ByteString Maybe Char
rCharL = StateT BSL.uncons
IL(rCharS) :: StateT BS.ByteString Maybe Char
rCharS = StateT BS.uncons
IL(rCharWL) :: StateT BSL.ByteString Maybe Word8
rCharWL = StateT BSLW.uncons
IL(rCharWS) :: StateT BS.ByteString Maybe Word8
rCharWS = StateT BSW.uncons
IL(dropSpecialL) :: (MonadState BSL.ByteString m) => m ()
dropSpecialL = modify $ BSLW.dropWhile (< ord8 '!')
IL(dropSpecialS) :: (MonadState BS.ByteString m) => m ()
dropSpecialS = modify $ BSW.dropWhile (< ord8 '!')
IL(skipSpecialL) :: (MonadState BSL.ByteString m) => m a -> m a
skipSpecialL = (dropSpecialL *>)
IL(skipSpecialS) :: (MonadState BS.ByteString m) => m a -> m a
skipSpecialS = (dropSpecialS *>)

IL(linToMat) :: (VG.Vector v a) => Int -> Int -> v a -> V.Vector (v a)
linToMat h w lvec = vEvalElemsId $ V.generate h (\i -> VG.slice (i*w) w lvec)

IL(mLinToMat) :: (VGM.MVector v a) => Int -> Int -> v s a -> V.Vector (v s a)
mLinToMat h w lvec = vEvalElemsId $ V.generate h (\i -> VGM.slice (i*w) w lvec)
  
IL(unsafeAddrToSVec) :: Int -> Addr# -> VS.Vector Word8
unsafeAddrToSVec n addr
  = (`VS.unsafeFromForeignPtr0` n)
    $ unsafeDupablePerformIO
    $ newForeignPtr_ $ Ptr addr

IL(vEvalElemsId) :: (VG.Vector v a) => v a -> v a
vEvalElemsId = vMapFoldl (\ !_ !x -> (x,())) ()

IL(vEvalElems) :: (VG.Vector v a) => v a -> ()
vEvalElems = VG.foldl' (\ !_ !_ -> ()) () 

IL(vMapFoldl) :: (VG.Vector v b, VG.Vector v c) =>
  (a -> b -> (c,a)) -> a -> v b -> v c
vMapFoldl f a
  = VG.unstream . VFB.inplace (streamMapFoldl f a) id . VG.stream

streamMapFoldl :: (Functor m) =>
  (a -> b -> (c,a)) -> a -> VFSM.Stream m b -> VFSM.Stream m c
{-# INLINE_FUSED streamMapFoldl #-}
streamMapFoldl f a (VFSM.Stream step s) = VFSM.Stream step1 (a,s)
  where
    {-# INLINE_INNER step1 #-}
    step1 (a0,s0) =  (<$> step s0) $ \r -> case r of
      VFSM.Yield b s1 -> case f a0 b of (c,a1) -> VFSM.Yield c (a1,s1)
      VFSM.Skip    s1 -> VFSM.Skip (a0,s1)
      VFSM.Done       -> VFSM.Done

IL(svecToBS) :: VS.Vector Word8 -> BS.ByteString
svecToBS vec = BSU.fromForeignPtr ptr 0 len
  where (ptr, len) = VS.unsafeToForeignPtr0 vec

IL(vLength) :: VG.Vector v a => v a -> Int
vLength = VFB.length . VG.stream

unlessM, whenM :: (Monad m) => m Bool -> m () -> m ()
IL(whenM) = (. flip when) . (>>=)
IL(unlessM) = (. flip unless) . (>>=)

IL(wrA) = A.writeArray
IL(rdA) = A.readArray
IL(mdA) = \arr f !i -> do
  ai <- rdA arr i
  let fai = f ai 
  wrA arr i fai
  return (ai,fai)
{-# INLINE mdA' #-}
mdA' = \arr f !i -> do
  !ai <- rdA arr i
  let !fai = f ai
  wrA arr i fai
  return (ai,fai)
IL(swapA) = \arr !i !j -> do
  ai <- rdA arr i
  wrA arr i =<< rdA arr j
  wrA arr j ai

#define D(f,r,d)\
  IL(f) :: Integral a=>a->d; f=fromIntegral;\
  IL(r) :: String->d; r=read
#define C(f,r,g,h,d) D(f,r,d);\
  g,h :: RealFrac a=>a->d; IL(g)=floor; IL(h)=ceiling
C(_toInteger_,readInteger,floorInteger,ceilInteger,Integer)
C(toInt,readInt,floorInt,ceilInt,Int)
C(toI8,readI8,floorI8,ceilI8,Int8)
C(toI16,readI16,floorI16,ceilI16,Int16)
C(toI32,readI32,floorI32,ceilI32,Int32)
C(toI64,readI64,floorI64,ceilI64,Int64)
C(toWord,readWord,floorWord,ceilWord,Word)
C(toW8,readW8,floorW8,ceilW8,Word8)
C(toW16,readW16,floorW16,ceilW16,Word16)
C(toW32,readW32,floorW32,ceilW32,Word32)
C(toW64,readW64,floorW64,ceilW64,Word64)
D(toDouble,readDouble,Double)
D(toFloat,readFloat,Float)
#undef D
#undef C

#define TS(f,a,m,init)\
  IL(f) :: forall e i s. (C(a,m) A.Ix i) => (i,i) -> init m (a i e); f
#define N(f,g,h,a,m)\
  TS(f,a,m,e->)=A.newArray;\
  TS(g,a,m,)=A.newArray_;\
  TS(h,a,m,[e]->)=A.newListArray
#define C(a,m)
N(newIOA,newIOA_,newIOAL,IOArray,IO)
N(newSTA,newSTA_,newSTAL,STArray s,ST s)
#undef C
#define C(a,m) MArray (a) e (m), 
N(newIOUA,newIOUA_,newIOUAL,IOUArray,IO)
N(newSTUA,newSTUA_,newSTUAL,STUArray s,ST s)
#undef C
#undef N
#undef TS

#undef IL

