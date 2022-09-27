{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}

module TransparentNewcomb where

import Bayes
import Data.Ord
import Data.List
import Data.Finitary
import GHC.Generics (Generic)
import Prelude hiding ((>>=), (>>), return)


data Agent = AlwaysOneBox | AlwaysTwoBox | EmptyOneFullTwo | FullOneEmptyTwo deriving stock (Eq, Generic, Show) deriving anyclass (Finitary)
data Doing = OneBoxing | TwoBoxin deriving stock (Eq, Generic, Show) deriving anyclass (Finitary)
data Prediction = WillOneBox | WillTwoBox deriving stock (Eq, Generic, Show) deriving anyclass (Finitary)
data Box = Empty | Full deriving stock (Eq, Generic, Show) deriving anyclass (Finitary)
data Action = OneBox | TwoBox deriving stock (Eq, Generic, Show) deriving anyclass (Finitary)
data Value = HundredOne | Hundred | One | Zero deriving stock (Eq, Generic, Show, Ord) deriving anyclass (Finitary)

-- Computing the value from a prediction and an action.
value :: Action -> Box -> Value
value OneBox Full = Hundred
value TwoBox Full = HundredOne
value OneBox Empty = Zero
value TwoBox Empty = One

-- The predictor.
predictor :: Agent -> Prediction
predictor AlwaysOneBox = WillOneBox
predictor AlwaysTwoBox = WillTwoBox
predictor EmptyOneFullTwo = WillTwoBox
predictor FullOneEmptyTwo = WillOneBox

-- The almost-perfect predictor.
almostPredictor :: Agent -> Distribution Prediction
almostPredictor AlwaysOneBox = Distribution $ \prediction -> case prediction of WillOneBox -> 99 ; WillTwoBox -> 1 
almostPredictor AlwaysTwoBox = Distribution $ \prediction -> case prediction of WillTwoBox -> 99 ; WillOneBox -> 1 
almostPredictor EmptyOneFullTwo = Distribution $ \prediction -> case prediction of WillTwoBox -> 99 ; WillOneBox -> 1
almostPredictor FullOneEmptyTwo = Distribution $ \prediction -> case prediction of WillOneBox -> 99 ; WillTwoBox -> 1

-- Box preparation.
box :: Prediction -> Box
box WillOneBox = Full
box WillTwoBox = Empty

-- Evaluate the action of an agent.
act :: Agent -> Box -> Action
act AlwaysOneBox _  = OneBox
act AlwaysTwoBox _  = TwoBox
act EmptyOneFullTwo Empty = OneBox 
act EmptyOneFullTwo Full  = TwoBox 
act FullOneEmptyTwo Empty = TwoBox 
act FullOneEmptyTwo Full  = OneBox 

-- Expected value
expected :: Distribution Value -> Rational
expected u = 
    let (Distribution d) = normalize u in 
      d Hundred * 1000 +
      d HundredOne * 1001 +
      d One

-- Predisposition.
predisposition :: Distribution Agent
predisposition = Distribution $ \agent ->
    case agent of
        AlwaysOneBox -> 1
        AlwaysTwoBox -> 1
        EmptyOneFullTwo -> 1
        FullOneEmptyTwo -> 1



-- Argmax method.
argmax :: (Agent -> Distribution Value) -> Agent
argmax f = maximumBy (comparing (expected . f)) inhabitants

-- Functional decision theory.
fdtModel :: Agent -> Distribution Value
fdtModel f = do
    agent <- return fdt
    boxes <- return (box (predictor agent))
    action <- return (act f boxes)
    return (value action boxes)

fdt :: Agent
fdt = argmax fdtModel

fdtThinks :: Agent -> Rational
fdtThinks f = expected (fdtModel f)


-- Causal decision theory
cdtModel :: Agent -> Distribution Value
cdtModel f = do
    agent <- predisposition
    boxes <- return (box (predictor agent))
    action <- return (act f boxes)
    return (value action boxes)

cdt :: Agent
cdt = argmax cdtModel

cdtThinks :: Agent -> Rational
cdtThinks f = expected (cdtModel f)


-- Evidential decision theory
edtModel :: Agent -> Distribution Value
edtModel f = do
    agent <- predisposition
    prediction <- almostPredictor agent
    boxes <- return (box prediction)
    action <- return (act agent boxes)
    observe (boxes == Empty)
    observe (action == act f Empty)
    return (value action boxes)

edt :: Agent
edt = argmax edtModel

edtThinks :: Agent -> Rational
edtThinks f = expected (edtModel f)
