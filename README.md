# semantic-tableaux

`Parser.py` constructs an ADT representations of parsed formulas from KB. 
This is then fed into Haskell which constructs the semantic tableaux (tree).

To run:
`python Parser.py <path_to_kb> | runhaskell Main.hs`
or directly:
`./do.sh <path_to_kb>`.
