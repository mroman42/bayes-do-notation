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

ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y 

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
{- data Colour = Red | Blue
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
  
  
-- Twin's prisoners dilemma
data Move = Cooperate | Defect
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Finitary)
data Outcomes = High | Medium | Low | None  
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Finitary)
  
uniform :: Distribution Move
uniform = Distribution $ \move ->
  case move of
    Cooperate -> 1 / 2
    Defect -> 1 / 2
    
value :: Move -> Move -> Outcomes
value Cooperate Cooperate = Medium
value Cooperate Defect    = None
value Defect    Cooperate = High
value Defect    Defect    = Low

twinprisoners :: Distribution Outcomes
twinprisoners = do
  me <- return Cooperate
  mytwin <- uniform
  observe (me == mytwin)
  return $ value me mytwin
  
-- Newcomb's problem
data Stage = OneBox | TwoBox
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Finitary)
data Doing = OneBoxing | TwoBoxing
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Finitary)
data Money = Million | Zero | One | MillionOne 
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Finitary)
    
predictor :: Distribution Stage
predictor = Distribution $ \stage ->
  case stage of
    OneBox -> 1 / 2
    TwoBox -> 1 / 2
    
you :: Distribution Doing
you = return TwoBoxing

money :: Doing -> Stage -> Money
money OneBoxing OneBox = Zero
money TwoBoxing OneBox = One
money OneBoxing TwoBox = Million
money TwoBoxing TwoBox = MillionOne

correctPrediction :: Doing -> Stage -> Bool
correctPrediction OneBoxing OneBox = False
correctPrediction TwoBoxing OneBox = True
correctPrediction OneBoxing TwoBox = True
correctPrediction TwoBoxing TwoBox = False

newcomb :: Distribution Money
newcomb = do
  stage <- predictor
  choice <- you
  observe (correctPrediction choice stage)
  return $ money choice stage
 -}