module Tableaux where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Control.Monad (guard)

import Prop

-- a tableaux is a tree with nodes = sets of propositions and their respective sat (i.e. ~closed)
type NodeType = (Set Phi, Bool)
type Tableaux = Tree NodeType

data SetStatus = NonAtomic | Open | Closed deriving (Eq, Show)

tableaux :: Phi -> Tableaux
tableaux sp = (build . Set.singleton) sp

{-
the tree is built as follows:
check the status of current set of formulas (sp)

if the set is open or closed, don't construct further (leaf node), 
set closed status (sat <-> ~closed)

if the set contains non-atomic elements,
select such an element (m) from the non-atomic-only subset,
delete it from the original sp (-> sp'),
and recursively construct the rest of the tree, branching on m
(depending on what formula m is)
the status of current node is an and of the all child statuses

if sat, a possible assignment is the Open, Atomic set
-}
build :: Set Phi -> Tableaux
build sp = case describeSet sp of
    Open      -> Tree.Node (sp, False) []
    Closed    -> Tree.Node (sp, True)  []
    NonAtomic -> Tree.Node (sp, cls) rest
    where
        (nonAtoms, _) = Set.partition (not . isAtom) sp
        m    = Set.findMin nonAtoms
        sp'  = Set.delete m sp
        rest = map (build . Set.union sp') (branchOn m)
        cls  = and $ map isNodeClosed rest

-- atom checks
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


-- Closed -> ∃ p, ~p in the set
describeSet :: Set Phi -> SetStatus
describeSet sp = if existsComplements sp 
    then Closed
    else if (not . allAtoms) sp then NonAtomic else Open


-- ops: beta / alpha
beta :: [Phi] -> [Set Phi]
beta xs = map Set.singleton xs

alpha :: [Phi] -> [Set Phi]
alpha xs = [Set.fromList xs]

branchOn :: Phi -> [Set Phi]
-- no ramification, just _expand_ the formula
branchOn (And p q)       = alpha [p, q]
branchOn (Not (Or p q))  = alpha [Not p, Not q]
branchOn (Not (Imp p q)) = alpha [p, Not q]
branchOn (Iff p q)       = alpha [Imp p q, Imp q p]
branchOn (Not (Not p))   = alpha [p]
-- ramification
branchOn (Or p q)        = beta [p, q]
branchOn (Imp p q)       = beta [Not p, q]
branchOn (Not (And p q)) = beta [Not p, Not q]
branchOn (Not (Iff p q)) = beta [Not (Imp p q), Not (Imp q p)]


-- eye-candy
closedStr True  = " {X} "
closedStr False = " {O} "

showNode :: NodeType -> String
showNode (sp, cls) = show (Set.toList sp) ++ (closedStr cls)

-- nodes -> string nodes with showNode, then show all tree
showTableaux :: Tableaux -> String
showTableaux t = (Tree.drawTree . fmap showNode) t
