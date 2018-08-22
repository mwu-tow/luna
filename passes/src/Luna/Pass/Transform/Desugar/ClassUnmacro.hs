{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Transform.Desugar.ClassUnmacro where

import Prologue

import qualified Data.Graph.Data.Component.List   as ComponentList
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Layer.Layout     as Layout
import qualified Data.Map                         as Map
import qualified Data.Mutable.Class               as Mutable
import qualified Data.Vector.Storable.Foreign     as Vector
import qualified Luna.IR                          as IR
import qualified Luna.IR.Aliases                  as Uni
import qualified Luna.IR.Layer                    as Layer
import qualified Luna.IR.Term.Ast.Invalid         as Invalid
import qualified Luna.Pass                        as Pass
import qualified Luna.Pass.Attr                   as Attr
import qualified Luna.Pass.Basic                  as Pass
import qualified Luna.Pass.Data.Stage             as TC
import qualified Luna.Pass.Data.UniqueNameGen     as NameGen
import qualified Luna.Syntax.Text.Lexer.Symbol    as Syntax

import Data.Map            (Map)



data ClassUnmacro

type instance Pass.Spec ClassUnmacro t
   = ClassUnmacroSpec t

type family ClassUnmacroSpec t where
    ClassUnmacroSpec t = Pass.BasicPassSpec t



unmacro :: IR.SomeTerm -> TC.Pass ClassUnmacro ()
unmacro e = do
    app <- listifyApp e
    case app of
        (name : _) -> Layer.read @IR.Model name >>= \case
            Uni.Var v -> case v of
                "def"   -> mkDef e app
                "class" -> mkClass e app
                _       -> mkInvalid e
            _         -> mkInvalid e
        [] -> mkInvalid e


listifyApp :: IR.SomeTerm -> TC.Pass ClassUnmacro [IR.SomeTerm]
listifyApp e = Layer.read @IR.Model e >>= \case
    Uni.App f' arg' -> do
        arg <- IR.source arg'
        f   <- listifyApp =<< IR.source f'
        return $ f <> [arg]
    _ -> do
        inv <- IR.invalid' Invalid.FunctionHeader
        return [inv]

mkDefName :: IR.SomeTerm -> TC.Pass ClassUnmacro IR.SomeTerm
mkDefName name = Layer.read @IR.Model name >>= \case
    Uni.Var{} -> return name
    _         -> IR.invalid' Invalid.FunctionHeader

mkDefArgs :: IR.SomeTerm -> TC.Pass ClassUnmacro [IR.SomeTerm]
mkDefArgs = irListToTerms Invalid.FunctionHeader

mkDefBody :: IR.SomeTerm -> TC.Pass ClassUnmacro IR.SomeTerm
mkDefBody body = return body

mkClassName :: IR.SomeTerm -> TC.Pass ClassUnmacro (Maybe IR.Name)
mkClassName name = Layer.read @IR.Model name >>= \case
    Uni.Cons n _ -> return $ Just n
    _            -> return Nothing

mkClassArgs :: IR.SomeTerm -> TC.Pass ClassUnmacro [IR.SomeTerm]
mkClassArgs = irListToTerms Invalid.FunctionHeader

mkClassCons :: IR.SomeTerm -> TC.Pass ClassUnmacro [IR.SomeTerm]
mkClassCons = irListToTerms Invalid.FunctionBlock

mkClassFields :: IR.SomeTerm -> TC.Pass ClassUnmacro [IR.SomeTerm]
mkClassFields = irListToTerms Invalid.FunctionBlock

irListToTerms :: Invalid.Symbol -> IR.SomeTerm -> TC.Pass ClassUnmacro [IR.SomeTerm]
irListToTerms invalid list = Layer.read @IR.Model list >>= \case
    Uni.List s -> Mutable.toList s >>= mapM IR.source
    _          -> do
        inv <- IR.invalid' invalid
        return [inv]

mkInvalid :: IR.SomeTerm -> TC.Pass ClassUnmacro ()
mkInvalid orig = do
    inv <- IR.invalid Invalid.FunctionHeader
    IR.replace inv orig

mkDef :: IR.SomeTerm -> [IR.SomeTerm] -> TC.Pass ClassUnmacro ()
mkDef orig app = case app of
    [_def, name, args, body] -> do
        name' <- mkDefName name
        args' <- mkDefArgs args
        body' <- mkDefBody body
        def   <- IR.function name' args' body'
        IR.replace def orig
    _ -> mkInvalid orig

mkClass :: IR.SomeTerm -> [IR.SomeTerm] -> TC.Pass ClassUnmacro ()
mkClass orig app = case app of
    [_class, name, args, conses, fields] -> do
        name' <- mkClassName name
        args' <- mkClassArgs args
        cons' <- mkClassCons conses
        flds' <- mkClassFields fields
        case name' of
            Just n -> do
                cls   <- IR.record False n args' cons' flds'
                IR.replace cls orig
            _      -> mkInvalid orig
    _ -> mkInvalid orig
