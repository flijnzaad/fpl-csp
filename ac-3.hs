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
ac3 (p, True, [])      = (p, True, [])
ac3 (p@(CSP vars doms cons), True, ((varx,vary),rel):xs) = if stronglookup varx doms == newxdomain 
    then ac3 (p, not $ null newxdomain, xs) 
    else ac3 (CSP vars newdoms cons, True, newqueue) where
        newxdomain  = revise ((varx,vary),rel) (stronglookup varx doms) (stronglookup vary doms)
        newdoms     = newxdomain: delete (stronglookup varx doms) doms
        newqueue    = xs ++ filter (\(arc,rel) -> snd arc == varx) cons

stronglookup :: Variable -> [Domain] -> Domain 
stronglookup x v = let (Just y) = lookup x v in (x,y)

revise :: Constraint -> Domain -> Domain -> Domain 
revise (_ , []) (varx, _) _ = (varx, [])
revise (_, rel) (varx, []) _ = (varx, [])
revise (arc, rel) (varx, x:xs) (vary, ys) = if any (\y -> (x,y) `elem` rel) ys then appendtosnd x (revise (arc, rel) (varx, xs) (vary, ys)) else revise (arc, rel) (varx, xs) (vary, ys)
-- test case : revise ((100,101),[(x,y)| x<-[1..4], y<-[1..4], x==y]) (100,[1..3]) (101,[2..4])

appendtosnd :: Value -> Domain -> Domain
appendtosnd x (varx, xs) = (varx, x:xs)