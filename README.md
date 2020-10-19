# Semantic Tableaux

`Parser.py` constructs an ADT representations of parsed formulas from the Knowledge Base.
This is then fed into Haskell which constructs the semantic tableaux (tree).


## Usage
```bash
python Parser.py <path_to_kb> | runhaskell Main.hs
```

or directly:

```bash
./do.sh <path_to_kb>
```
