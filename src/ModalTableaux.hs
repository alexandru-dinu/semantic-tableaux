module Tableaux where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Control.Monad (guard)

import Prop

-- | A tableaux is a tree with nodes
-- | sets of propositions, their respective world id, and sat (i.e. ~closed)
type NodeType = (Set Phi, Integer, Bool)
type Tableaux = Tree NodeType

data SetStatus = NonAtomic | Open | Closed deriving (Eq, Show)

tableaux :: Phi -> Tableaux
tableaux sp = (build . Set.singleton) sp

{-
start with initial world 0
exhaust type 1 rules (simple ST + put ¬ inside for box and diamond)
construct new world (prev + 1) for each diamond p => p is true in this world
box q will be true in all succs => copied without box
also, because we are in K (p -> ☐p), all literals in a world are copied in the succs of that world
-}
build :: Set Phi -> Tableaux
build sp = case describeSet sp of
    Open      -> Tree.Node (sp, False) []
    Closed    -> Tree.Node (sp, True)  []
    NonAtomic -> Tree.Node (sp, wId, cls) rest
    where
        (nonAtoms, _) = Set.partition (not . isAtom) sp
        m    = Set.findMin nonAtoms
        sp'  = Set.delete m sp
        rest = map (build . Set.union sp') (branchOn m)
        cls  = and $ map isNodeClosed rest

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
    | otherwise        = if Set.member (complement m) sp' then True else existsComplements sp'
    where
        m   = Set.findMin sp
        sp' = Set.deleteMin sp


-- | Get the closed-state of a node
isNodeClosed :: Tableaux -> Bool
isNodeClosed (Tree.Node (_, _, c) _) = c


-- | Describe a set based on contained formulas
-- | Closed     <-> ∃ p, ~p in the set
-- | Open       <-> non-closed, atomic elements => sat
-- | NonAtomic  <-> ∃ f that can be further expanded
describeSet :: Set Phi -> SetStatus
describeSet sp = if existsComplements sp
    then Closed
    else if (not . allAtoms) sp then NonAtomic else Open


-- | Construction rules

-- | Construct separate subsets
beta :: [Phi] -> [Set Phi]
beta xs = map Set.singleton xs

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
showNode (sp, cls) = show (Set.toList sp) ++ (closedStr cls)

-- | nodes -> string nodes with showNode, then show all tree
showTableaux :: Tableaux -> String
showTableaux t = (Tree.drawTree . fmap showNode) t
