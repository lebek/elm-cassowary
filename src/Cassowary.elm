module Cassowary exposing (standardForm, doPivot, simplex)

{-| A library for building constraint-based UIs

# Constructing Constraints
@docs standardForm

# Solving
@docs doPivot, simplex

-}

import Cassowary.Equation exposing (..)
import Cassowary.Infix exposing (..)
import Dict
import Array
import List


-- there's a bug in elm's Dict.merge https://github.com/elm-lang/core/pull/648


merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> Dict.Dict comparable a
    -> Dict.Dict comparable b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    if lKey < rKey then
                        stepState rKey rValue ( rest, leftStep lKey lValue result )
                    else if lKey > rKey then
                        ( list, rightStep rKey rValue result )
                    else
                        ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            Dict.foldl stepState ( Dict.toList leftDict, initialResult ) rightDict
    in
        List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers


{-| EquationStdForm is a phantom data type. Don't instantiate it yourself, use
standardForm instead.
-}
type EquationStdForm
    = EqualityStdForm Expression
    | InequalityStdForm Expression


type alias BasicRows =
    Dict.Dict String EquationStdForm


type alias NonBasicRows =
    Array.Array EquationStdForm


type alias Tableau =
    { basic : BasicRows
    , nonBasic : NonBasicRows
    }


isEmpty : Dict.Dict comparable b -> Bool
isEmpty =
    List.isEmpty << Dict.keys



-- eliminates terms where coeff = 0


addDicts : Dict.Dict comparable number -> Dict.Dict comparable number -> Dict.Dict comparable number
addDicts a b =
    let
        c1 k v1 acc =
            Dict.insert k v1 acc

        c2 k v1 v2 acc =
            Dict.insert k (v1 + v2) acc

        c3 k v2 acc =
            Dict.insert k v2 acc
    in
        merge c1 c2 c3 a b Dict.empty
            |> Dict.filter (\k v -> v /= 0)


subtractDicts : Dict.Dict comparable number -> Dict.Dict comparable number -> Dict.Dict comparable number
subtractDicts a b =
    addDicts a <| Dict.map (\k v -> negate v) b



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
standardForm : Equation -> EquationStdForm
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
-- get the entering basic var (if any)


getEnteringVar : VariableMap -> Maybe String
getEnteringVar objective =
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


getLeavingVar : Array.Array EquationStdForm -> String -> Maybe Int
getLeavingVar tableau enteringVar =
    let
        ratios =
            Array.indexedMap (\i row -> ( i, blandRatio enteringVar row )) tableau

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
-- aka Minumum Ratio Test (MRT)


blandRatio : String -> EquationStdForm -> Maybe Float
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


divideRowByPivot : String -> EquationStdForm -> EquationStdForm
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


getVars stdForm =
    case stdForm of
        EqualityStdForm expr ->
            expr.vars

        InequalityStdForm expr ->
            expr.vars


getConst stdForm =
    case stdForm of
        EqualityStdForm expr ->
            expr.const

        InequalityStdForm expr ->
            expr.const


mapStdForm f stdForm =
    case stdForm of
        EqualityStdForm expr ->
            EqualityStdForm (f expr)

        InequalityStdForm expr ->
            InequalityStdForm (f expr)


removeVar v stdForm =
    mapStdForm
        (\eq ->
            { vars = Dict.remove v eq.vars
            , const = eq.const
            }
        )
        stdForm


type alias Pivot =
    { enteringVar : String
    , leavingIdx : Int
    , leavingRow : EquationStdForm
    }


andThen' =
    flip Maybe.andThen


getPivot : EquationStdForm -> BasicRows -> NonBasicRows -> Maybe Pivot
getPivot objective basicRows nonBasicRows =
    let
        both =
            (Array.append nonBasicRows <| Array.fromList <| Dict.values basicRows)
    in
        (getEnteringVar <| getVars objective)
            |> andThen'
                (\enteringVar ->
                    getLeavingVar both enteringVar
                        |> andThen'
                            (\leavingIdx ->
                                Array.get leavingIdx both
                                    |> Maybe.map
                                        (\leavingRow ->
                                            { enteringVar = enteringVar
                                            , leavingIdx = leavingIdx
                                            , leavingRow = leavingRow
                                            }
                                        )
                            )
                )


substituteEquation : Pivot -> EquationStdForm -> EquationStdForm -> EquationStdForm
substituteEquation pivot replacement equation =
    case Dict.get pivot.enteringVar (getVars equation) of
        Just coeff ->
            let
                scaled =
                    mapStdForm
                        (\e ->
                            { vars = Dict.map (\k v -> v * coeff) e.vars
                            , const = e.const * coeff
                            }
                        )
                        replacement
            in
                mapStdForm
                    (\e ->
                        { vars = subtractDicts e.vars (getVars scaled)
                        , const = e.const - (getConst scaled)
                        }
                    )
                    equation

        Nothing ->
            equation


substituteRows : Pivot -> EquationStdForm -> NonBasicRows -> NonBasicRows
substituteRows pivot replacement nonBasicRows =
    Array.map (substituteEquation pivot replacement) nonBasicRows


substituteObjective : Pivot -> EquationStdForm -> EquationStdForm -> EquationStdForm
substituteObjective pivot replacement objective =
    substituteEquation pivot replacement objective


arrayRemove : Int -> Array.Array a -> Array.Array a
arrayRemove idx array =
    Array.append (Array.slice 0 idx array)
        (Array.slice (idx + 1) ((Array.length array) - 1) array)



{-| Perform a pivot - transforms Tableau to new canonical form
-}
doPivot : EquationStdForm -> Tableau -> Maybe ( Tableau, EquationStdForm )
doPivot objective tableau =
    let
        pivot =
            getPivot objective tableau.basic tableau.nonBasic

        getReplacement pivot =
            divideRowByPivot pivot.enteringVar pivot.leavingRow

        replace pivot replacement =
            ( { basic = Dict.insert pivot.enteringVar (removeVar pivot.enteringVar replacement) <| Dict.map (\k v -> substituteEquation pivot replacement v) tableau.basic
              , nonBasic = substituteRows pivot replacement (arrayRemove pivot.leavingIdx tableau.nonBasic)
              }
            , substituteObjective pivot replacement objective
            )
    in
        pivot
            |> Maybe.map (\pivot -> getReplacement pivot |> replace pivot)


{-| Optimizes the given tableau
-}
simplex : EquationStdForm -> Tableau -> ( Tableau, EquationStdForm )
simplex objective tableau =
    case doPivot objective tableau of
        Just ( tableau, objective ) ->
            simplex objective tableau

        Nothing ->
            ( tableau, objective )
