--data types - constructors and pattern matching
data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4

--An underscore "_"  matches anything.
--syntaxis of "case"
case exp of
  pat1 -> exp1
  pat2 -> exp2
  ...
