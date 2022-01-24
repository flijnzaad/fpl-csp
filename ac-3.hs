-- we will probably need this at some point
import Data.List

type Variable = Int
type Variables = [Variable]
data CSP = Csp { vars :: Variables
               , doms :: Domains
               , cons :: Constraints }
