module Cassowary.Infix exposing (..)

import Cassowary.Equation exposing (..)


{-| This module contains infix operators for concise equation construction.

# Infix operators
@docs (.*.), (.+.), (.<=.), (.>=.), (.=.)

-}
(.+.) : ExpressionAst -> ExpressionAst -> ExpressionAst
(.+.) =
    Add


(.*.) : ExpressionAst -> Float -> ExpressionAst
(.*.) =
    Coeff


(.=.) : ExpressionAst -> ExpressionAst -> Equation
(.=.) x y =
    Equality (makeExpression x) (makeExpression y)


(.<=.) : ExpressionAst -> ExpressionAst -> Equation
(.<=.) x y =
    Inequality (makeExpression x) (makeExpression y)


(.>=.) : ExpressionAst -> ExpressionAst -> Equation
(.>=.) =
    flip (.<=.)


infixr 7 .<=.


infixr 7 .>=.


infixr 7 .=.


infixr 8 .+.


infixr 9 .*.
