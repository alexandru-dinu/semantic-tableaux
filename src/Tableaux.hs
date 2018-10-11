module Tableaux where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Control.Monad (guard)

import Prop

-- a tableaux is a tree with nodes = sets of propositions and their respective sat (i.e. !closed)
type NodeType = (Set Phi, Bool)
type Tableaux = Tree NodeType

tableaux :: Phi -> Tableaux
tableaux sp = (build . Set.singleton) sp

-- TODO: DOC!!!
build :: Set Phi -> Tableaux
build sp = case closed sp of
    -- closed from the beginning, leaf, we're done
    Just bool -> Tree.Node (sp, bool) [] 
    -- not closed, construct tree
    Nothing   -> Tree.Node (sp, cls) rest
    where
        (nonAtoms, _) = Set.partition (not . isAtom) sp
        m    = Set.findMin nonAtoms
        sp'  = Set.delete m sp
        rest = map (build . Set.union sp') (branchOn m)
        cls  = and $ map isNodeClosed rest

-- Var checks
isAtom :: Phi -> Bool
isAtom (Var _)       = True
isAtom (Not (Var _)) = True
isAtom _             = False

allAtoms :: Set Phi -> Bool
allAtoms sp = Set.null $ Set.filter (not . isAtom) sp


-- complement checks
complement :: Phi -> Phi
complement (Not p) = p
complement p       = Not p

existsComplements :: Set Phi -> Bool
existsComplements sp 
    | Set.size sp <= 1 = False
    | otherwise        = if Set.member (complement m) sp' then True else existsComplements sp'
    where
        m   = Set.findMin sp
        sp' = Set.deleteMin sp


-- closure checks
isNodeClosed :: Tableaux -> Bool
isNodeClosed (Tree.Node (_, c) _) = c

-- if all are atoms in set, then check if there are complements
-- guard returns pure () if True, else empty
closed :: Set Phi -> Maybe Bool
closed sp = do
    guard $ allAtoms sp
    return $ existsComplements sp

-- ops: split / goWith
split :: [Phi] -> [Set Phi]
split xs = map Set.singleton xs

goWith :: [Phi] -> [Set Phi]
goWith xs = [Set.fromList xs]

branchOn :: Phi -> [Set Phi]
-- no ramification, just _expand_ the formula
branchOn (And p q)       = goWith [p, q]
branchOn (Not (Or p q))  = goWith [Not p, Not q]
branchOn (Not (Imp p q)) = goWith [p, Not q]
branchOn (Iff p q)       = goWith [Imp p q, Imp q p]
branchOn (Not (Not p))   = goWith [p]
-- ramification
branchOn (Or p q)        = split [p, q]
branchOn (Imp p q)       = split [Not p, q]
branchOn (Not (And p q)) = split [Not p, Not q]
branchOn (Not (Iff p q)) = split [Not (Imp p q), Not (Imp q p)]


-- eye-candy
closedStr True  = " {X} "
closedStr False = " {O} "

showNode :: NodeType -> String
showNode (sp, cls) = show (Set.toList sp) ++ (closedStr cls)

-- nodes -> string nodes with showNode, then show all tree
showTableaux :: Tableaux -> String
showTableaux t = (Tree.drawTree . fmap showNode) t