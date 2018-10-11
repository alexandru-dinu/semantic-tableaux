module Prop where

import Data.Map (Map)
import qualified Data.Map as Map

type Context = Map String Bool

data Phi = Var String
    | Not Phi
    | And Phi Phi
    | Or  Phi Phi
    | Iff Phi Phi
    | Imp Phi Phi
    deriving (Read, Eq, Ord)

instance Show Phi where
    show (Var x)    =  x
    show (Not p)    = "~" ++ (show p)
    show (And p q)  = "(" ++ (show p) ++ " and " ++ (show q) ++ ")"
    show (Or p q)   = "(" ++ (show p) ++ " or " ++ (show q) ++ ")"
    show (Iff p q)  = "(" ++ (show p) ++ " <-> " ++ (show q) ++ ")"
    show (Imp p q)  = "(" ++ (show p) ++ " -> " ++ (show q) ++ ")"

iff :: Bool -> Bool -> Bool
iff = (==)

imp :: Bool -> Bool -> Bool
imp p q = (not p) || q


eval :: Context -> Phi -> Bool
eval ctx (Var x) = case Map.lookup x ctx of
    Just v  -> v
    Nothing -> error "DNE"
eval ctx (Not p)   = not $ eval ctx p
eval ctx (And p q) = (eval ctx p)   &&   (eval ctx q)
eval ctx (Or p q)  = (eval ctx p)   ||   (eval ctx q)
eval ctx (Iff p q) = (eval ctx p)  `iff` (eval ctx q)
eval ctx (Imp p q) = (eval ctx p)  `imp` (eval ctx q)

