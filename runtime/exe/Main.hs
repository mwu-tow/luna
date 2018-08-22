{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Prologue hiding (switch, putStr)
import Preluna.Interpreter as Interpreter
import Luna.IR (Name, Qualified)
import qualified Preluna.Std as Std
import qualified Luna.Runtime  as Luna
import qualified Control.Concurrent.Future as Future
import qualified Data.Map as Map

addModFun :: Qualified -> Name -> AST -> Luna.Units -> IO Luna.Units
addModFun mod n ast us = mfix $ \(~units) -> do
    r <- Future.make $ interpret' units ast
    pure $ wrap $ Map.union (unwrap us) (Map.singleton mod (Luna.Unit (Map.singleton n r) def))

infixr 0 <~
(<~) = Unify
infixr 4 $$
($$) = App
switch c t f = ((Def "Std.Base" "if.then.else" $$ c) $$ t) $$ f
true    = Cons "Std.Base" "Bool" "True" []
false   = Cons "Std.Base" "Bool" "False" []
putStr  = Def "Std.Base" "putStr"
minus   = Def "Std.Base" "intMinus"
plus    = Def "Std.Base" "intPlus"
lt      = Def "Std.Base" "intLt"
intShow = Def "Std.Base" "intShow"

{-main = do-}
    {-let progXd = Var 0 <~ Lam (Var 1) (switch ((lt $$ Number 0) $$ Var 1)-}
                                              {-((plus $$ Number 1) $$ Var 0 $$ (minus $$ Var 1) $$ Number 1)-}
                                              {-(Number 0)-}
                                              {-)-}
    {-print progXd-}

main = do
    progXD <- read =<< readFile "in.preluna"
    let prog2  = [ Var 0 <~ Def "X" "test" $$ Number 10000000
                 , putStr $$ intShow $$ Var 0
                 ]
    s <- addModFun "X" "test" progXD Std.std
    r <- interpret' s prog2
    print progXD
    Luna.runIO (Luna.force r)

