module Tableaux where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Control.Monad (guard)

import Prop

-- | A tableaux is a tree with nodes = sets of propositions and their respective sat (i.e. ~closed)
type NodeType = (Set Phi, Bool)
type Tableaux = Tree NodeType

data SetStatus = NonAtomic | Open | Closed deriving (Eq, Show)

tableaux :: Phi -> Tableaux
tableaux = build . Set.singleton

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
        cls  = all isNodeClosed rest

-- | Check if formulas are atomic
isAtom :: Phi -> Bool
isAtom (Var _)       = True
isAtom (Not (Var _)) = True
isAtom _             = False

allAtoms :: Set Phi -> Bool
allAtoms sp = Set.null $ Set.filter (not . isAtom) sp


-- | Check if there are complements in a set of formulas
complement :: Phi -> Phi
complement (Not p) = p
complement p       = Not p

existsComplements :: Set Phi -> Bool
existsComplements sp
    | Set.size sp <= 1 = False
    | otherwise        = Set.member (complement m) sp' || existsComplements sp'
    where
        m   = Set.findMin sp
        sp' = Set.deleteMin sp


-- | Get the closed-state of a node
isNodeClosed :: Tableaux -> Bool
isNodeClosed (Tree.Node (_, c) _) = c


-- | Describe a set based on contained formulas
-- | Closed     <-> ∃ p, ~p in the set
-- | Open       <-> non-closed, atomic elements => sat
-- | NonAtomic  <-> ∃ f that can be further expanded
describeSet :: Set Phi -> SetStatus
describeSet sp
    | existsComplements sp = Closed
    | (not . allAtoms) sp  = NonAtomic
    | otherwise            = Open


-- | Construction rules

-- | Construct separate subsets
beta :: [Phi] -> [Set Phi]
beta = map Set.singleton

-- | Construct a single set
alpha :: [Phi] -> [Set Phi]
alpha xs = [Set.fromList xs]

-- | Node expansion rules
branchOn :: Phi -> [Set Phi]

-- | No ramification, just _expand_ the formula
branchOn (And p q)       = alpha [p, q]
branchOn (Not (Or p q))  = alpha [Not p, Not q]
branchOn (Not (Imp p q)) = alpha [p, Not q]
branchOn (Iff p q)       = alpha [Imp p q, Imp q p]
branchOn (Not (Not p))   = alpha [p]

-- | Ramification
branchOn (Or p q)        = beta [p, q]
branchOn (Imp p q)       = beta [Not p, q]
branchOn (Not (And p q)) = beta [Not p, Not q]
branchOn (Not (Iff p q)) = beta [Not (Imp p q), Not (Imp q p)]

-- | Modal logic
branchOn (Not (Nec p))   = alpha [Pos (Not p)]
branchOn (Not (Pos p))   = alpha [Nec (Not p)]


-- | Tree printing utils
closedStr True  = " {X} "
closedStr False = " {O} "

showNode :: NodeType -> String
showNode (sp, cls) = show (Set.toList sp) ++ closedStr cls

-- | nodes -> string nodes with showNode, then show all tree
showTableaux :: Tableaux -> String
showTableaux = Tree.drawTree . fmap showNode
