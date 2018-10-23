from enum import Enum

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
        ret = "  " * level + repr(self.sp) + " " + Tree._show(self.closed) + "\n"
        for kid in self.kids:
            ret += kid.__str__(level+1)
        return ret

    @staticmethod
    def _show(b):
        return "{X}" if b else "{O}"



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

def tableaux(phi):
    return build([phi])

def build(sp):
    desc = describeSet(sp)

    if desc == SetStatus.Open:
        return Tree(sp, False, [])

    if desc == SetStatus.Closed:
        return Tree(sp, True, [])

    if desc == SetStatus.NonAtomic:
        nonAtoms, _ = partition(lambda p : not p.isAtom(), sp)
        m = findMin(nonAtoms)
        sp2 = copy(sp)
        sp2.remove(m)
        rest = list(map(lambda x : build(sp2 + x), branchOn(m)))
        closed = reduce(lambda x, y: x and y, map(lambda n : n.closed, rest), True)
        return Tree(sp, closed, rest)

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

    if getName(phi) == 'Or':
        return beta([phi.p1, phi.p2])
    if getName(phi) == 'Imp':
        return beta([Not(phi.p1), phi.p2])
    if getName(phi) == 'Not' and getName(phi.p) == 'And':
        return beta([Not(phi.p.p1), Not(phi.p.p2)])
    if getName(phi) == 'Not' and getName(phi.p) == 'Iff':
        return beta([Not(Imp(phi.p.p1, phi.p.p2)), Not(Imp(phi.p.p2, phi.p.p1))])
