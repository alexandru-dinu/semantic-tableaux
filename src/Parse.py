import sys
from lark import Lark, Transformer, v_args


# TODO: 
# NOT only works on atoms
# convert natural language to prop (a,b,c etc)

grammar = """
    start: phi                  -> do_phi

    phi: "NOT" ATOM             -> do_not
        | phi "OR" phi          -> do_or
        | phi "AND" phi         -> do_and
        | phi "IFF" phi         -> do_iff
        | "IF" phi "THEN" phi   -> do_imp
        | ATOM                  -> do_atom

    ATOM: /[a-z_]+/

    %import common.WS_INLINE
    %ignore WS_INLINE
"""


@v_args(inline=True)
class PLTransformer(Transformer):
    def __init__(self):
        self.vs = {}

    # final formula
    def do_phi(self, p):
        # print(p)
        return p

    def do_not(self, p):
        # phi = "~(" + p + ")"
        phi = "Not (Var \"" + p + "\")"
        return phi

    def do_or(self, p1, p2):
        # phi = "(" + p1 + " or " + p2 + ")"
        phi = "Or (" + p1 + ") (" + p2 + ")"
        return phi
    
    def do_and(self, p1, p2):
        # phi = "(" + p1 + " and " + p2 + ")"
        phi = "And (" + p1 + ") (" + p2 + ")"
        return phi

    def do_iff(self, p1, p2):
        # phi = "(" + p1 + " <-> " + p2 + ")"
        phi = "Iff (" + p1 + ") (" + p2 + ")"
        return phi
    
    def do_imp(self, p1, p2):
        # phi = "(" + p1 + " -> " + p2 + ")"
        phi = "Imp (" + p1 + ") (" + p2 + ")"
        return phi

    def do_atom(self, p):
        return "Var \"" + p + "\""

pl_parser = Lark(grammar, parser='lalr', transformer=PLTransformer())


def cons_and(ps):
    if len(ps) == 2:
        p, q = ps
        return "And (" + p + ") (" + q + ")"
    return "And (" + ps[0] + ") (" + cons_and(ps[1:]) + ")"

def main(kb):
    props = [line.strip() for line in open(kb, "rt").readlines()]
    props = [pl_parser.parse(p) for p in props]
    phi = cons_and(props)

    print(phi)

if __name__ == '__main__':
    if len(sys.argv) != 2:
        while True:
            pl_parser.parse(input("> "))
    else:
        main(sys.argv[1])
