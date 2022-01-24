import Data.List -- we will probably need this at some point

type Variable   = Int
type Value      = Int
type Domain     = (Variable, [Value])
type Arc        = (Variable, Variable)
type Constraint = ( Arc, [(Value, Value)] )
data Problem    = CSP { vars :: [Variable]
                      , doms :: [Domain]
                      , cons :: [Constraint] }

ac3 :: (Problem, Bool, [Constraint]) -> (Problem, Bool, [Constraint])
ac3 (p, False, _)   = (p, False, [])
ac3 (p, _, [])      = (p, True, [])
ac3 (p, _, (x:xs))  = (p, True, xs) 


revise :: Constraint -> Domain -> Domain -> Domain 
revise (_ , []) (varx, _) _ = (varx, [])
revise (_, rel) (varx, []) _ = (varx, [])
revise (arc, rel) (varx, (x:xs)) (vary, ys) = if any (\y -> (x,y) `elem` rel) ys then appendtosnd x (revise (arc, rel) (varx, xs) (vary, ys)) else revise (arc, rel) (varx, xs) (vary, ys)
-- test case : revise ((100,101),[(x,y)| x<-[1..4], y<-[1..4], x==y]) (100,[1..3]) (101,[2..4])

appendtosnd :: Value -> Domain -> Domain
appendtosnd x (varx, xs) = (varx, x:xs)