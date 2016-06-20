module Cassowary.Equation exposing (
  ExpressionAst(..), Expression, Equation(..), makeExpression, VariableMap)

{-| Todo

# Definition
@docs ExpressionAst

-}

import Dict
import Array

{-| User-level Linear Expression AST -}
type ExpressionAst =
    Var String
  | Lit Float
  | Coeff ExpressionAst Float
  | Add ExpressionAst ExpressionAst

type alias VariableMap = Dict.Dict String Float

{-| Internal Linear Expression representation -}
type alias Expression =
  { vars  : VariableMap
  , const : Float
  }

{-| Internal Linear Equation representation -}
type Equation =
    Equality Expression Expression
  | Inequality Expression Expression -- by convention always lte, ie e1 <= e2

{-| Converts a user-level AST to our internal canonical Linear Expression
representation.
-}
extendExpression : Expression -> ExpressionAst -> Expression
extendExpression expr ast = case (expr, ast) of
  ({vars, const}, Var n) -> Expression (Maybe.withDefault
    (Dict.insert n 1 vars)
    (Maybe.map (\coeff -> Dict.insert n (coeff+1) vars)
    (Dict.get n vars))) const
  ({vars, const}, Lit n) -> Expression vars (const + n)
  (lexpr, Coeff _ 0) -> lexpr
  ({vars, const}, Coeff (Var n) x) -> Expression (Maybe.withDefault
    (Dict.insert n x vars)
    (Maybe.map (\coeff -> Dict.insert n (coeff+x) vars)
    (Dict.get n vars))) const
  (lexpr, Add e1 e2) -> extendExpression (extendExpression lexpr e2) e1
  (_, _) -> Debug.crash "Unhandled expression"

makeExpression : ExpressionAst -> Expression
makeExpression = extendExpression { vars = Dict.empty, const = 0 }
