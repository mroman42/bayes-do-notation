-- A tiny probabilistic language using rebindable do-notation. We can do
-- Bayesian update using the observe declaration.
--
-- Author: Mario Roman.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Bayes where

import Prelude hiding ((>>=), (>>), return)
import Data.Finitary
import GHC.Generics (Generic)

-- Finitary subdistribution monad.
data Distribution a where
  Distribution :: (Finitary a) => (a -> Rational) -> Distribution a

-- Properties of a distribution.
total :: Distribution a -> Rational
total (Distribution d) = sum [ d x | x <- inhabitants ]

weight :: Distribution a -> a -> Rational
weight (Distribution f) a = f a

-- Normalization here is just a pretty printing thing.
normalize :: Distribution a -> Distribution a
normalize (Distribution f) = Distribution $ \a ->
  f a / (total (Distribution f))
instance (Finitary a, Show a) => Show (Distribution a) where
  show d =
    let (Distribution f) = normalize d in
      unlines [ show (f a, a) | a <- inhabitants ]


-- Rebindable do notation.
(>>=) :: (Finitary a , Finitary b) => Distribution a -> (a -> Distribution b) -> Distribution b
(>>=) (Distribution d) f = Distribution $ \b ->
  sum $ [ (d a) * (weight (f a) b) | a <- inhabitants]

(>>) :: (Finitary a, Finitary b) => Distribution a -> Distribution b -> Distribution b
(>>) d (Distribution f) = Distribution $ \b -> (total d) * (f b)

fail :: Distribution a -> String
fail = undefined

-- Distribution combinators.
return :: (Finitary a) => a -> Distribution a
return x = Distribution (\y ->
  case (x == y) of
    True -> 1
    False -> 0)

absurd :: (Finitary a) => Distribution a
absurd = Distribution (\a -> 0)

observe :: Bool -> Distribution ()
observe True = return ()
observe False = absurd

-- Example.
data Colour = Red | Blue
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Finitary)
data Urn = UrnRed | UrnMixed | UrnBlue
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Finitary)

content :: Urn -> Distribution Colour
content UrnRed = return Red
content UrnBlue = return Blue
content UrnMixed = Distribution $ \colour ->
  case colour of
    Blue -> 0.5
    Red -> 0.5

prior :: Distribution Urn
prior = Distribution $ \urn ->
  case urn of
    UrnRed -> 1
    UrnBlue -> 1
    UrnMixed -> 1

example :: Distribution Urn
example = do
  urn <- prior
  ball <- content urn
  observe (ball == Red)
  ball <- content urn
  observe (ball == Red)
  return urn
