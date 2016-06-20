module Cassowary exposing (standardForm)

{-| A library for building constraint-based UIs

# Constructing Constraints
@docs standardForm

-}

import Cassowary.Equation exposing (..)
import Cassowary.Infix exposing (..)
import Dict
import Array


{-| EquationStdForm is a phantom data type. Don't instantiate it yourself, use
standardForm instead.
-}
type EquationStdForm a
    = EqualityStdForm Expression
    | InequalityStdForm Expression


type alias Tableau =
    Array.Array Expression


isEmpty : Dict.Dict comparable b -> Bool
isEmpty =
    List.isEmpty << Dict.keys


subtractDicts : Dict.Dict comparable number -> Dict.Dict comparable number -> Dict.Dict comparable number
subtractDicts a b =
    let
        c1 k v1 acc =
            Dict.insert k v1 acc

        c2 k v1 v2 acc =
            Dict.insert k (v1 - v2) acc

        c3 k v2 acc =
            Dict.insert k -v2 acc
    in
        Dict.merge c1 c2 c3 a b Dict.empty



-- vars on one side, consts on the other


rebalance : Equation -> Equation
rebalance expr =
    case expr of
        Equality a b ->
            if (isEmpty a.vars) then
                Equality { vars = Dict.empty, const = (a.const - b.const) } { vars = b.vars, const = 0 }
            else if (isEmpty b.vars) then
                Equality { vars = a.vars, const = 0 } { vars = Dict.empty, const = (b.const - a.const) }
            else
                Equality { vars = subtractDicts a.vars b.vars, const = 0 } { vars = Dict.empty, const = (b.const - a.const) }

        Inequality a b ->
            if (isEmpty a.vars) then
                Inequality { vars = Dict.empty, const = (a.const - b.const) } { vars = b.vars, const = 0 }
            else if (isEmpty b.vars) then
                Inequality { vars = a.vars, const = 0 } { vars = Dict.empty, const = (b.const - a.const) }
            else
                Inequality { vars = subtractDicts a.vars b.vars, const = 0 } { vars = Dict.empty, const = (b.const - a.const) }


{-| Reduce equations to standard form expressions via implicit slack variables.
Note that we drop the direction of the inequality here but it doesn't matter
because it's accounted for by signage of slack coefficient.
-}
standardForm : Equation -> EquationStdForm a
standardForm expr =
    case rebalance expr of
        Equality a b ->
            if (isEmpty a.vars) then
                EqualityStdForm { vars = b.vars, const = a.const }
            else if (isEmpty b.vars) then
                EqualityStdForm { vars = a.vars, const = b.const }
            else
                Debug.crash "Can't find standard form"

        Inequality a b ->
            if (isEmpty a.vars) then
                InequalityStdForm { vars = b.vars, const = a.const }
            else if (isEmpty b.vars) then
                InequalityStdForm { vars = a.vars, const = b.const }
            else
                Debug.crash "Can't find standard form"



-- SIMPLEX
-- minimum negative objective coefficient, else Nothing


nextBasic : VariableMap -> Maybe String
nextBasic objective =
    let
        f k v acc =
            case acc of
                Nothing ->
                    if (v < 0) then
                        Just ( k, v )
                    else
                        Nothing

                Just ( k1, v1 ) ->
                    if (v < 0) && (v < v1) then
                        Just ( k, v )
                    else
                        Just ( k1, v1 )
    in
        Maybe.map fst <| Dict.foldr f Nothing objective



-- the row index with the minimum bland ratio, else Nothing


nextRow : Array.Array (EquationStdForm a) -> String -> Maybe Int
nextRow tableau b =
    let
        ratios =
            Array.indexedMap (\i row -> ( i, blandRatio b row )) tableau

        f ( idx, ratio ) acc =
            case ( ratio, acc ) of
                ( Just ratio, Just ( accI, accR ) ) ->
                    if ratio < accR then
                        Just ( idx, ratio )
                    else
                        acc

                ( Nothing, _ ) ->
                    acc

                ( Just ratio, Nothing ) ->
                    Just ( idx, ratio )
    in
        Maybe.map fst <| Array.foldr f Nothing ratios



-- convert number of Maybe, where Nothing if less than 0


nonposToNothing : number -> Maybe number
nonposToNothing n =
    if n > 0 then
        Just n
    else
        Nothing



-- compute bland ratio for a row
-- (pivot coefficient) / (row constant)
-- row constant must be strictly positive, else Nothing


blandRatio : String -> EquationStdForm a -> Maybe Float
blandRatio var row =
    let
        getCoeff expr =
            Maybe.andThen (Dict.get var expr.vars) nonposToNothing

        ratio expr =
            Maybe.map (\cf -> expr.const / cf) (getCoeff expr)
    in
        case row of
            EqualityStdForm expr ->
                ratio expr

            InequalityStdForm expr ->
                ratio expr



-- | Divide entire row by a pivot elements, first step in pivot


divideRowByPivot : String -> EquationStdForm a -> EquationStdForm b
divideRowByPivot pivotVar row =
    let
        refactor expr =
            case Dict.get pivotVar expr.vars of
                Just coeff ->
                    { vars = Dict.map (\k v -> v / coeff) expr.vars
                    , const = expr.const / coeff
                    }

                Nothing ->
                    Debug.crash "`divideRowByPivot` called with pivot var not in row"
    in
        case row of
            EqualityStdForm expr ->
                EqualityStdForm <| refactor expr

            InequalityStdForm expr ->
                InequalityStdForm <| refactor expr



-- pivot : VariableMap -> Tableau -> Maybe number
-- pivot objective slack = ?
