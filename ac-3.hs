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
ac3 (p, False, _) = (p, False, [])
ac3 (p, True, []) = (p, True,  [])
ac3 (p@(CSP vars doms cons), True, ((varX, varY), rel):xs) =
  if strongLookup varX doms == newXDomain
    then ac3 (p, not $ null newXDomain, xs)
    else ac3 (CSP vars newDoms cons, True, newQueue) where
        newXDomain = revise ((varX, varY), rel) (strongLookup varX doms) (strongLookup varY doms)
        newDoms    = newXDomain: delete (strongLookup varX doms) doms
        newQueue   = xs ++ filter (\(arc, rel) -> snd arc == varX) cons

strongLookup :: Variable -> [Domain] -> Domain
strongLookup x v = let (Just y) = lookup x v in (x,y)

-- test case : revise ((100,101),[(x,y)| x<-[1..4], y<-[1..4], x==y]) (100,[1..3]) (101,[2..4])
revise :: Constraint -> Domain -> Domain -> Domain
revise (_ , []) (varX,  _) _ = (varX, [])
revise (_, rel) (varX, []) _ = (varX, [])
revise (arc, rel) (varX, x:xs) (varY, ys) =
  if any (\y -> (x, y) `elem` rel) ys
     then appendToSnd x (revise (arc, rel) (varX, xs) (varY, ys))
     else revise (arc, rel) (varX, xs) (varY, ys)

appendToSnd :: Value -> Domain -> Domain
appendToSnd x (varX, xs) = (varX, x:xs)
