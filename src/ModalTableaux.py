from enum import Enum
from operator import itemgetter
from itertools import tee, filterfalse
from copy import copy
from functools import reduce

from Prop import *

def partition(pred, iterable):
    t1, t2 = tee(iterable)
    r1 = list(filter(pred, t1))
    r2 = list(filterfalse(pred, t2))
    return r1, r2

def findMin(sp):
    xs = map(lambda p : (p, p.getPrio()), sp)
    xs = sorted(xs, key=itemgetter(1))
    return xs[0][0]

def getName(x):
    return x.__class__.__name__


class SetStatus(Enum):
    NonAtomic = 1
    Open      = 2
    Closed    = 3


class Tree:
    def __init__(self, sp, closed, kids):
        self.sp     = sp
        self.closed = closed
        self.kids   = kids

    def __repr__(self):
        return "<semantic tableaux>"

    def __str__(self, level=0):
        ret = "  " * level + repr(self.sp) + " " + _show(self.closed) + "\n"
        for kid in self.kids:
            ret += kid.__str__(level+1)
        return ret

def _show(b):
    return "{X}" if b else "{O}"

def isSat(tree):
    return not tree.closed


def tableaux(phi):
    return build([phi])

def allAtoms(sp):
    return len(list(filter(lambda x : not x.isAtom(), sp))) == 0


def complement(p):
    return p.p if getName(p) == "Not" else Not(p)

def existsComplements(sp):
    if len(sp) <= 1:
        return False
    else:
        m = findMin(sp)
        r = copy(sp)
        r.remove(m)
        if complement(m) in r:
            return True
        else:
            return existsComplements(r)

def describeSet(sp):
    if existsComplements(sp):
        return SetStatus.Closed
    if not allAtoms(sp):
        return SetStatus.NonAtomic
    else:
        return SetStatus.Open

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
