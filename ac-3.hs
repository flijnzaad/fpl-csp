import Data.List -- we will probably need this at some point

type Variable   = Int
type Value      = Int
type Domain     = (Variable, [Value])
type Constraint = ( (Variable, Variable), (Value, Value) )
data Problem    = CSP { vars :: [Variable]
                      , doms :: [Domain]
                      , cons :: [Constraint] }
