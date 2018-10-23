class Phi:
    def __init__(self):
        pass

    def __repr__(self):
        raise NotImplementedError

    def evaluate(self):
        raise NotImplementedError

    def isAtom(self):
        return False

    def getPrio(self):
        raise NotImplementedError


class Var(Phi):
    def __init__(self, v):
        self.v = v

    def __repr__(self):
        return self.v

    def evaluate(self, context):
        return context[self.v]

    def isAtom(self):
        return True

    def getPrio(self):
        return 1

    def __eq__(self, other):
        if isinstance(other, Var):
            return self.v == other.v
        return False


class Not(Phi):
    def __init__(self, p):
        self.p = p

    def __repr__(self):
        return "~" + repr(self.p)

    def evaluate(self, context):
        return not self.p.evaluate(context)

    def isAtom(self):
        return self.p.isAtom()

    def getPrio(self):
        return 2

    def __eq__(self, other):
        if isinstance(other, Not):
            return self.p == other.p
        return False


class And(Phi):
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2

    def __repr__(self):
        return "(" + repr(self.p1) + " ^ " + repr(self.p2) + ")"

    def evaluate(self, context):
        r1 = self.p1.evaluate(context)
        r2 = self.p2.evaluate(context)
        return (r1 and r2)

    def getPrio(self):
        return 3

    def __eq__(self, other):
        if isinstance(other, And):
            return (self.p1 == other.p1) and (self.p2 == other.p2)
        return False



class Or(Phi):
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2

    def __repr__(self):
        return "(" + repr(self.p1) + " V " + repr(self.p2) + ")"

    def evaluate(self, context):
        r1 = self.p1.evaluate(context)
        r2 = self.p2.evaluate(context)
        return (r1 or r2)

    def getPrio(self):
        return 4

    def __eq__(self, other):
        if isinstance(other, Or):
            return (self.p1 == other.p1) and (self.p2 == other.p2)
        return False


class Iff(Phi):
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2

    def __repr__(self):
        return "(" + repr(self.p1) + " <-> " + repr(self.p2) + ")"

    def evaluate(self, context):
        r1 = self.p1.evaluate(context)
        r2 = self.p2.evaluate(context)
        return (r1 == r2)

    def getPrio(self):
        return 5

    def __eq__(self, other):
        if isinstance(other, Iff):
            return (self.p1 == other.p1) and (self.p2 == other.p2)
        return False


class Imp(Phi):
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2

    def __repr__(self):
        return "(" + repr(self.p1) + " -> " + repr(self.p2) + ")"

    def evaluate(self, context):
        r1 = self.p1.evaluate(context)
        r2 = self.p2.evaluate(context)
        return ((not r1) or r2)

    def getPrio(self):
        return 6

    def __eq__(self, other):
        if isinstance(other, Imp):
            return (self.p1 == other.p1) and (self.p2 == other.p2)
        return False


class Pos(Phi):
    def __init__(self, p):
        self.p = p

    def __repr__(self):
        return "◇ " + repr(self.p)

    def evaluate(self, context):
        pass

    def getPrio(self):
        return 7

    def __eq__(self, other):
        if isinstance(other, Pos):
            return (self.p == other.p)
        return False


class Nec(Phi):
    def __init__(self, p):
        self.p = p

    def __repr__(self):
        return "☐ " + repr(self.p)

    def evaluate(self, context):
        pass

    def getPrio(self):
        return 8

    def __eq__(self, other):
        if isinstance(other, Nec):
            return (self.p == other.p)
        return False
