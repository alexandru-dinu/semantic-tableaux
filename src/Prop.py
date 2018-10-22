class Phi:
    def __init__(self):
        pass
    def __repr__(self):
        raise NotImplementedError
    def evaluate(self):
        raise NotImplementedError

class Var(Phi):
    def __init__(self, v):
        self.v = v
    def __repr__(self):
        return self.v
    def evaluate(self, context):
        return context[self.v]

class Not(Phi):
    def __init__(self, p):
        self.p = p
    def __repr__(self):
        return "~" + repr(self.p)
    def evaluate(self, context):
        return not self.p.evaluate(context)

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

class Nec(Phi):
    def __init__(self, p):
        self.p = p
    def __repr__(self):
        return "☐ " + repr(self.p)
    def evaluate(self, context):
        pass

class Pos(Phi):
    def __init__(self, p):
        self.p = p
    def __repr__(self):
        return "◇ " + repr(self.p)
    def evaluate(self, context):
        pass
