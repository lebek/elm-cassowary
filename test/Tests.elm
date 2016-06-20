module Tests exposing (all)

import ElmTest exposing (..)
import Cassowary.Equation exposing (..)
import Cassowary.Infix exposing (..)
import Cassowary exposing (standardForm)
import Dict


astSuite : Test
astSuite =
    suite "AST tests"
        [ test "Simple" <| (Var "x" .*. 2) `assertEqual` (Coeff (Var "x") 2) ]


assertExpressionsEqual expr1 expr2 =
    let
        getVars =
            Dict.toList << .vars
    in
        assert ((getVars expr1) == (getVars expr2) && expr1.const == expr2.const)


makeExpressionSuite : Test
makeExpressionSuite =
    let
        check ast expression =
            assertExpressionsEqual (makeExpression ast) expression
    in
        suite "Standard Form tests"
            [ test ""
                <| check (Add (Add (Coeff (Var "x") 2) (Var "y")) (Lit 4))
                    { vars = Dict.fromList [ ( "x", 2 ), ( "y", 1 ) ]
                    , const = 4
                    }
            ]


all : Test
all =
    suite "Combine test suite"
        [ astSuite
        , makeExpressionSuite
        ]


main =
    runSuite all
