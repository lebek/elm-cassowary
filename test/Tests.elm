module Tests exposing (all)

import ElmTest exposing (..)
import Cassowary.Equation exposing (..)
import Cassowary.Infix exposing (..)
import Cassowary exposing (..)

import Dict
import Array

astSuite : Test
astSuite =
    suite "AST tests"
        [ test "Simple" <|  (Coeff (Var "x") 2) `assertEqual` (Var "x" .*. 2) ]


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
            [ test "Simple"
                <| check (Add (Add (Coeff (Var "x") 2) (Var "y")) (Lit 4))
                    { vars = Dict.fromList [ ( "x", 2 ), ( "y", 1 ) ]
                    , const = 4
                    }
            ]

makeProgram constraints objective = ({
    basic=Dict.empty
  , nonBasic=(Array.fromList <| List.map standardForm constraints)
  }, standardForm <| objective)

-- Ex 2.1 Linear Programming: Foundations and Extensions
-- (x1,x2,x3,x4) = (2,0,1,0), ζ = 17.
problem1 = makeProgram [
    Var "x1" .*. 2 .+. Var "x2" .+. Var "x3" .+. Var "x4" .*. 3 .<=. Lit 5
  , Var "x1" .+. Var "x2" .*. 3 .+. Var "x3" .+. Var "x4" .*. 2 .<=. Lit 3
  ]
  (Var "p" .=. Var "x1" .*. 6 .+. Var "x2" .*. 8 .+. Var "x3" .*. 5 .+. Var "x4" .*. 9)


-- Ex 2.2 Linear Programming: Foundations and Extensions
-- (x1,x2) = (1,0), ζ = 2.
problem2 = makeProgram [
    Var "x1" .*. 2 .+. Var "x2" .<=. Lit 4
  , Var "x1" .*. 2 .+. Var "x2" .*. 3 .<=. Lit 3
  , Var "x1" .*. 4 .+. Var "x2" .<=. Lit 5
  , Var "x1" .+. Var "x2" .*. 5 .<=. Lit 1
  ]
  (Var "p" .=. Var "x1" .*. 2 .+. Var "x2" .*. 8)

pivotSuite : Test
pivotSuite = suite "Pivot tests"
    [
      test "Simple" <| problem2 `assertEqual` (simplex (snd problem2) (fst problem2))
    ]

all : Test
all =
    suite "Combine test suite"
        [ astSuite
        , makeExpressionSuite
        , pivotSuite
        ]


main =
    runSuite all
