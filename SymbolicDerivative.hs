module SymbolicDerivative where 
    
import Utils.DataTypes
import Utils.ToString
import Utils.Derivative
import Utils.Simplify
import Utils.ComprExpr
import Utils.Parser

-- wersje funkcji dla ExprThree opakowanych w monadę Maybe 
-- - takie są uzyskiwane jako wynik funkcji parse
allDerivativesM :: Maybe ExprThree -> Maybe (ExprThree, ExprThree, ExprThree)
allDerivativesM expr = expr >>= ((Just ) . allDerivatives)

derivativeM :: Maybe ExprThree -> Var -> Maybe ExprThree
derivativeM expr v = expr >>= ((Just ) . ((flip derivative) v))

simplifyM :: Maybe ExprThree -> Maybe ExprThree
simplifyM expr = expr >>= (Just ) . simplify