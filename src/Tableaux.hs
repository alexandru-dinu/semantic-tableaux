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

tableaux :: Phi -> Tableaux
tableaux sp = (build . Set.singleton) sp

{-
the tree is built as follows:
check the closedStatus of current set of formulas (sp)
    if the set contains only atomic elements, 
    construct a leaf node containing sp and its status (sat <-> ~closed)

    if the set contains non-atomic elements,
    select such an element (m) from the non-atomic-only subset,
    delete it from the original sp (-> sp'),
    and recursively construct the rest of the tree, branching on m
    (depending on what formula m is)
    the closedStatus of current node is an and of the all child statuses
-}
build :: Set Phi -> Tableaux
build sp = case closedStatus sp of
    -- sp is atomic -> leaf and its 'closed' status
    Just cls -> Tree.Node (sp, cls) [] 
    -- sp is non-atomic -> select + expand formula
    Nothing   -> Tree.Node (sp, cls) rest
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

{-
Nothing -> there are non-atomic elements in the set 
(i.e. they can be further expanded)
Just cls -> if all elements are atomic, check for complements
(i.e. cls == True <-> ∃ p, ~p in the set)
-}
closedStatus :: Set Phi -> Maybe Bool
closedStatus sp = do
    guard $ allAtoms sp
    return $ existsComplements sp


-- ops: separate / extract
separate :: [Phi] -> [Set Phi]
separate xs = map Set.singleton xs

extract :: [Phi] -> [Set Phi]
extract xs = [Set.fromList xs]

branchOn :: Phi -> [Set Phi]
-- no ramification, just _expand_ the formula
branchOn (And p q)       = extract [p, q]
branchOn (Not (Or p q))  = extract [Not p, Not q]
branchOn (Not (Imp p q)) = extract [p, Not q]
branchOn (Iff p q)       = extract [Imp p q, Imp q p]
branchOn (Not (Not p))   = extract [p]
-- ramification
branchOn (Or p q)        = separate [p, q]
branchOn (Imp p q)       = separate [Not p, q]
branchOn (Not (And p q)) = separate [Not p, Not q]
branchOn (Not (Iff p q)) = separate [Not (Imp p q), Not (Imp q p)]


-- eye-candy
closedStr True  = " {X} "
closedStr False = " {O} "

showNode :: NodeType -> String
showNode (sp, cls) = show (Set.toList sp) ++ (closedStr cls)

-- nodes -> string nodes with showNode, then show all tree
showTableaux :: Tableaux -> String
showTableaux t = (Tree.drawTree . fmap showNode) t
