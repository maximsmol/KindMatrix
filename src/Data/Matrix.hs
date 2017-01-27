{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Matrix where

import Prelude hiding (zipWith)

import GHC.TypeLits
import qualified Data.List as L
import Data.Semigroup
import Data.Proxy

import qualified Data.Vector as V

import Util.NatValue

newtype Matrix (h :: Nat) (w :: Nat) a = Matrix {
	mData :: V.Vector a
} deriving (Eq)

height :: forall h w a b. (KnownNat h, Num b) => Matrix h w a -> b
height _ = natValue @h

width :: forall h w a b. (KnownNat w, Num b) => Matrix h w a -> b
width _ = natValue @w

row :: forall y h w a. (KnownNat y, KnownNat w) => Matrix h w a -> V.Vector a
row =
	let
		y = natValue @y
		w = natValue @w
	in V.slice (y*w) w . mData

col :: forall x h w a. (KnownNat x, KnownNat h, KnownNat w) => Matrix h w a -> V.Vector a
col =
	let
		x = natValue @x
		w = natValue @w
		h = natValue @h
	in V.fromList . map (V.! x) . take h . iterate (snd . V.splitAt w) . mData

instance Functor (Matrix h w) where
	fmap f = Matrix . fmap f . mData

instance (Show a, KnownNat h, KnownNat w) => Show (Matrix h w a) where
	show = mShow . mData
		where
			mShow :: V.Vector a -> String
			mShow v =
				let
					h = natValue @h
					w = natValue @w
				in concat . L.intersperse "\n" . fmap (show . V.take w) . take h . iterate (snd . V.splitAt w) $ v

instance (KnownNat h, KnownNat w, Semigroup a) => Semigroup (Matrix w h a) where
	(<>) = zipWith (<>)

instance (KnownNat h, KnownNat w, Semigroup a, Monoid a) => Monoid (Matrix w h a) where
	mempty = generate $ const mempty
	mappend = (<>)

instance (KnownNat h, KnownNat w, Num a) => Num (Matrix w h a) where
	negate = fmap negate
	abs = fmap abs
	signum = fmap signum
	fromInteger i = fmap (fromInteger.(* i)) identity

	a + b = fmap getSum $ fmap Sum a <> fmap Sum b
	a * b = fmap calcEntry $ generate id
		where
			calcEntry :: (Int, Int) -> a
			calcEntry (x, y) =
				case someNatVal (fromIntegral y) of
					Just (SomeNat (_ :: Proxy y)) ->
						case someNatVal (fromIntegral x) of
							Just (SomeNat (_ :: Proxy x)) ->
								sum $ V.zipWith (*) (row @x a) (col @y b)
							_ -> error "assert: x >= 0"
					_ -> error "assert: y >= 0"

identity :: (KnownNat h, KnownNat w, Num b) => Matrix h w b
identity = generate (\(x, y) -> if x == y then 1 else 0)

generate :: forall h w b. (KnownNat h, KnownNat w) => ((Int, Int) -> b) -> Matrix h w b
generate f =
	let
		h = natValue @h
		w = natValue @w

		rowcol x = (x `div` w, x `mod` w)
	in Matrix $ V.generate (h*w) (f . rowcol)

zipWith :: (a -> b -> c) -> Matrix h w a -> Matrix h w b -> Matrix h w c
zipWith f a b = Matrix $ V.zipWith f (mData a) (mData b)

test1 :: Matrix 4 4 Int
test1 = generate $ \(i,j) -> 2*(i+1) - (j+1)
test2 :: Matrix 4 4 Int
test2 = generate $ \(i,j) -> 4*(i+1) - 2*(j+1)
test3 :: Matrix 3 4 Int
test3 = generate $ \(i,j) -> 10*(i+1) + (j+1)
