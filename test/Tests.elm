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
        [ test "Simple" <| (Coeff (Var "x") 2) `assertEqual` (Var "x" .*. 2) ]


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


makeProgram constraints objective =
    ( { basic = Dict.empty
      , nonBasic = (Array.fromList <| List.map standardForm constraints)
      }
    , standardForm <| objective
    )



-- Ex 2.1 Linear Programming: Foundations and Extensions


problem1 =
    makeProgram
        [ Var "x1" .*. 2 .+. Var "x2" .+. Var "x3" .+. Var "x4" .*. 3 .<=. Lit 5
        , Var "x1" .+. Var "x2" .*. 3 .+. Var "x3" .+. Var "x4" .*. 2 .<=. Lit 3
        ]
        (Var "p" .=. Var "x1" .*. 6 .+. Var "x2" .*. 8 .+. Var "x3" .*. 5 .+. Var "x4" .*. 9)

solution1 = Dict.fromList [
  ("x1", 2), ("x2",0), ("x3", 1), ("x4",0), ("p", 17)]


-- Ex 2.2 Linear Programming: Foundations and Extensions


problem2 =
    makeProgram
        [ Var "x1" .*. 2 .+. Var "x2" .<=. Lit 4
        , Var "x1" .*. 2 .+. Var "x2" .*. 3 .<=. Lit 3
        , Var "x1" .*. 4 .+. Var "x2" .<=. Lit 5
        , Var "x1" .+. Var "x2" .*. 5 .<=. Lit 1
        ]
        (Var "p" .=. Var "x1" .*. 2 .+. Var "x2" .*. 8)

solution2 = Dict.fromList [
  ("x1", 1), ("x2", 0), ("p", 2)]

-- Ex 2.3 Linear Programming: Foundations and Extensions


problem3 =
    makeProgram
        [ Var "x1" .*. (-1) .+. Var "x2" .*. (-1) .+. Var "x3" .*. (-1) .<=. Lit (-2)
        , Var "x1" .*. 2 .+. Var "x2" .*. (-1) .+. Var "x3" .<=. Lit 1
        ]
        (Var "p" .=. Var "x1" .*. 2 .+. Var "x2" .*. (-6))

solution3 = Dict.fromList [
  ("x1", 0), ("x2", 0.5), ("x3", 1.5), ("p", -3)]

assertResultsEqual result1 result2 =
  assertEqual (Dict.toList result1) (Dict.toList result2)

testProblem problem solution =
  assertResultsEqual solution ((uncurry simplexResult) <| (uncurry simplex) problem)

pivotSuite : Test
pivotSuite =
    suite "Pivot tests"
        [ test "Ex 2.1" <| testProblem problem1 solution1
        , test "Ex 2.2" <| testProblem problem2 solution2
        , test "Ex 2.3" <| testProblem problem3 solution3
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
