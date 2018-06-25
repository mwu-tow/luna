module Luna.Pass.Data.UniqueNameGen where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr

newtype UniqueNameGen = UniqueNameGen [String]
type instance Attr.Type UniqueNameGen = Attr.Atomic
instance Default UniqueNameGen where
    def = UniqueNameGen allNames

allNames :: [String]
allNames = ('a' :) . show <$> [0..]

generateName :: Attr.Editor UniqueNameGen m => m IR.Name
generateName = do
    UniqueNameGen (n : ns) <- Attr.get
    Attr.put (UniqueNameGen ns)
    return $ convert n