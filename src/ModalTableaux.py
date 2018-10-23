from enum import Enum
from termcolor import colored
from copy import copy
from functools import reduce

from Prop import *
from Utils import partition, findMin, getName



class SetStatus(Enum):
    NonAtomic = 1
    Open      = 2
    Closed    = 3


class Tree:
    def __init__(self, sp, closed, kids, world=0):
        self.sp     = sp
        self.closed = closed
        self.kids   = kids
        self.world  = world

    def __repr__(self):
        return "<semantic tableaux>"

    def __str__(self, level=0):
        ret = "  " * level  + repr(self.sp) + \
            colored(" <" + repr(self.world) + "> ", 'cyan') + \
            Tree._show(self.closed) + "\n"
        for kid in self.kids:
            ret += kid.__str__(level+1)
        return ret

    @staticmethod
    def _show(b):
        return colored("{X}", 'red') if b else colored("{O}", 'green')



def isSat(tree):
    return not tree.closed

def allAtoms(sp):
    return len(list(filter(lambda x : not x.isAtom(), sp))) == 0

def complement(p):
    return p.p if getName(p) == "Not" else Not(p)

def existsComplements(sp):
    if len(sp) <= 1:
        return False

    m = findMin(sp)
    r = copy(sp)
    r.remove(m)
    return True if complement(m) in r else existsComplements(r)

def describeSet(sp):
    if existsComplements(sp):
        return SetStatus.Closed
    if not allAtoms(sp):
        return SetStatus.NonAtomic
    else:
        return SetStatus.Open


"""
w0: [p1, ..., pn]
- exhaust type-1 rules
- for each diamond(p) -> create new world where p is true
- box(q) will be copied in all newly created worlds
- K: p -> box(p), so all literals are also copied in all newly created worlds

in world w, exhaust all type1 rules

in world w, for each diamond(phi) in sp: --type2
    construct new world wi
    put phi in wi
    put all box(psi), p from sp in wi --type3
    link wi with w

    recursively on spi in wi
"""

def isModal(p):
    return (isDiamond(p) or isBox(p))

def allModal(sp):
    for p in sp:
        if not isModal(p):
            return False
    return True

def isDiamond(phi):
    return (getName(phi) == 'Pos')

def isBox(phi):
    return (getName(phi) == 'Nec')

def tableaux(phi):
    return build([phi])

def build(sp, world=0):
    desc = describeSet(sp)

    if desc == SetStatus.Open:
        return Tree(sp, False, [], world)

    if desc == SetStatus.Closed:
        return Tree(sp, True, [], world)

    if desc == SetStatus.NonAtomic:
        nonAtoms, atoms = partition(lambda p : not p.isAtom(), sp)

        # type1 rules were exhausted
        if allModal(nonAtoms):
            root = Tree(sp, False, [], world)

            # type2 -- world creating rules
            diamonds = [p for p in nonAtoms if isDiamond(p)]
            for i, d in enumerate(diamonds):
                new_sp = [d.p] + [x.p for x in nonAtoms if isBox(x)] + atoms # --type3 rules
                rest = build(new_sp, world+i+1)
                node = Tree(new_sp, rest.closed, [rest], rest.world)
                root.kids += [node]

            root.closed = reduce(lambda x, y: x and y, map(lambda n : n.closed, root.kids), True)
            return root

        # type1 rules can still be applied
        else:
            m = findMin(nonAtoms) # guaranteed to return a non-modal phi
            sp2 = copy(sp)
            sp2.remove(m)
            rest = list(map(lambda x : build(sp2 + x, world), branchOn(m)))
            closed = reduce(lambda x, y: x and y, map(lambda n : n.closed, rest), True)
            return Tree(sp, closed, rest, world)

def alpha(sp):
    return [sp]

def beta(sp):
    return list(map(lambda x : [x], sp))

def branchOn(phi):
    if getName(phi) == 'And':
        return alpha([phi.p1, phi.p2])
    if getName(phi) == 'Not' and getName(phi.p) == 'Or':
        return alpha([Not(phi.p.p1), Not(phi.p.p2)])
    if getName(phi) == 'Not' and getName(phi.p) == 'Imp':
        return alpha([phi.p.p1, Not(phi.p.p2)])
    if getName(phi) == 'Iff':
        return alpha([Imp(phi.p1, phi.p2), Imp(phi.p2, phi.p1)])
    if getName(phi) == 'Not' and getName(phi.p) == 'Not':
        return alpha([phi.p.p])
    if getName(phi) == 'Not' and getName(phi.p) == 'Pos':
        return alpha([Nec(Not(phi.p.p))])
    if getName(phi) == 'Not' and getName(phi.p) == 'Nec':
        return alpha([Pos(Not(phi.p.p))])

    if getName(phi) == 'Or':
        return beta([phi.p1, phi.p2])
    if getName(phi) == 'Imp':
        return beta([Not(phi.p1), phi.p2])
    if getName(phi) == 'Not' and getName(phi.p) == 'And':
        return beta([Not(phi.p.p1), Not(phi.p.p2)])
    if getName(phi) == 'Not' and getName(phi.p) == 'Iff':
        return beta([Not(Imp(phi.p.p1, phi.p.p2)), Not(Imp(phi.p.p2, phi.p.p1))])
