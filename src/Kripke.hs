module Kripke where

import Data.Map (Map)
import qualified Data.Map as Map

import Prop    

type World      = String
type Worlds     = [World]
type Relations  = [(World, World)] -- wRv :: v is accessible from w
type Valuations = Map World [Phi]
    
-- A Kripke Model is a tuple M = <W, R, V>
data Model = Model {
    worldsOf     :: Worlds,
    relationsOf  :: Relations,
    valuationsOf :: Valuations
} deriving Show


-- get all worlds accessible from w, in model m
accessibleFrom :: World -> Model -> Worlds
accessibleFrom w m = snd $ unzip $ filter (\(u,v) -> u == w) $ relationsOf m


-- evaluate formula f under the Model m, in World w
evaluate :: Model -> World -> Phi -> Bool

-- eval p
evaluate model world (Var v) = case Map.lookup world $ valuationsOf model of 
    Just ps -> elem (Var v) ps
    Nothing -> False 

-- eval ~p
evaluate model world (Not p) = not $ evaluate model world p

-- eval p ^ q
evaluate model world (And p q) = (r1 && r2) where
    r1 = evaluate model world p
    r2 = evaluate model world q

-- eval p v q
evaluate model world (Or p q) = (r1 || r2) where
    r1 = evaluate model world p
    r2 = evaluate model world q

-- eval p -> q
evaluate model world (Imp p q) = evaluate model world f
    where f = Or (Not p) q

-- eval p <-> q
evaluate model world (Iff p q) = evaluate model world f
    where f = And (Imp p q) (Imp q p)

-- eval ☐ p
evaluate model world (Nec p) = and $ map eval' (accessibleFrom world model)
    where eval' w = evaluate model w p

-- eval ◇ p
evaluate model world (Pos p) = evaluate model world (Not (Nec (Not p)))


-- TESTS

-- Kripke Model 1
w1 = ["w0", "w1", "w2", "w3"]
r1 = [("w0", "w1"), ("w0", "w2"), ("w1", "w3"), ("w2", "w3"), ("w1", "w2")]
v1 = Map.fromAscList [
    ("w0", [Not (Var "p"), Var "q"]), 
    ("w1", [Var "p", Not (Var "q")]),
    ("w2", [Not (Var "p"), Not (Var "q")]), 
    ("w3", [Var "p", Var "q"])]
m1 = Model w1 r1 v1
-- //

-- Kripke Model 2
w2 = ["w0", "w1", "w2", "w3"]
r2 = [("w0", "w1"), ("w0", "w2"), ("w0", "w3")]
v2 = Map.fromAscList [
    ("w0", [Var "D", Var "W"]), 
    ("w1", [Var "D", Var "W", Var "F", Var "B", Not (Var "C")]),
    ("w2", [Var "D", Var "W", Not (Var "F"), Var "B", Var "C"]), 
    ("w3", [Var "D", Var "W", Not (Var "F"), Var "B", Not (Var "C")])]
m2 = Model w2 r2 v2
-- //

-- Test Formulas
f1 = Pos $ Var "p"
f2 = Nec $ Not $ Var "q"
f3 = Nec $ Nec $ Var "p"
f4 = Pos $ Pos $ Var "p"
f5 = Pos $ Pos $ Var "q"

g1 = Nec $ Imp (Var "D") (Or (Var "C") (Var "B"))
g2 = Nec $ Imp (Var "W") (Or (Var "F") (Var "B"))
g3 = Not $ Pos $ And (Var "C") (Var "F")
g4 = Nec $ Var "B"

g5 = Iff (And (Nec (Var "B")) (Pos (Var "C"))) (And (Pos (Var "B")) (Nec (Var "C")))
-- //