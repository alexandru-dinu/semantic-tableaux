from operator import itemgetter
from itertools import tee, filterfalse

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
