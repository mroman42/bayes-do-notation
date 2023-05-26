{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Smoking where

import Bayes
import Data.Ord
import Data.List hiding (repeat)
import Data.Finitary
import Control.Monad hiding ((>>=), (>>), return)
import GHC.Generics (Generic)
import Prelude hiding ((>>=), (>>), return, repeat)


data Model = SmokingCausesCancer | GenesCauseCancer deriving stock (Eq, Generic, Show) deriving anyclass (Finitary)
data Genes = NormalGenes | SmokerGenes deriving stock (Eq, Generic, Show) deriving anyclass (Finitary)
data IsSmoker = Smoker | NonSmoker deriving stock (Eq, Generic, Show) deriving anyclass (Finitary)
data IsHealthy = Cancer | Healthy deriving stock (Eq, Generic, Show) deriving anyclass (Finitary)

population :: Distribution Genes
population = Distribution $ \case
    NormalGenes -> 2
    SmokerGenes -> 1

smokingBehaviour :: Genes -> Distribution IsSmoker
smokingBehaviour NormalGenes = Distribution $ \case
    Smoker -> 1
    NonSmoker -> 2
smokingBehaviour SmokerGenes = Distribution $ \case
    Smoker -> 4
    NonSmoker -> 1

cancer :: Model -> Genes -> IsSmoker -> Distribution IsHealthy
cancer SmokingCausesCancer _ Smoker = Distribution $ \case  
    Cancer -> 1
    Healthy -> 1
cancer SmokingCausesCancer _ NonSmoker = Distribution $ \case
    Cancer -> 1
    Healthy -> 9
cancer GenesCauseCancer NormalGenes _ = Distribution $ \case  
    Cancer -> 1
    Healthy -> 9
cancer GenesCauseCancer SmokerGenes _ = Distribution $ \case
    Cancer -> 1
    Healthy -> 1

repeat :: Int -> Distribution () -> Distribution ()
repeat 0 process = return ()
repeat n process = do
    process
    repeat (n-1) process



experiment :: Distribution Model
experiment = do
    -- We give the same probability to the smoking and genes model.
    model <- Distribution $ \case
        SmokingCausesCancer -> 1
        GenesCauseCancer -> 1

    -- Given an individual, we force them not to smoke, we observe they are healthy.
    -- We do so five times.
    repeat 5 $ do
        individual <- population
        smokes <- smokingBehaviour individual
        health <- cancer model individual NonSmoker
        observe (health == Healthy)

    -- Given an individual, we force them not to smoke, again.
    individual <- population
    smokes <- smokingBehaviour individual
    health <- cancer model individual NonSmoker

    observe (health == Healthy)

    return model


