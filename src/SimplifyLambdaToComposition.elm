module SimplifyLambdaToComposition exposing (rule)

{-| Converts lambdas like `(\x -> func x.field)` to `(func << .field)`

Also handles nested applications like `(\x -> f (g x.field))` -> `(f << g << .field)`

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports lambdas that can be simplified to function composition.

    config =
        [ SimplifyLambdaToComposition.rule
        ]


## Fail

    List.sortBy (\todo -> Time.posixToMillis todo.createdAt) todos

    List.map (\x -> f (g x.field)) list


## Success

    List.sortBy (Time.posixToMillis << .createdAt) todos

    List.map (f << g << .field) list


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template alex-tan/elm-review-opinionated-simplify/example --rules SimplifyLambdaToComposition
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "SimplifyLambdaToComposition" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor node =
    case Node.value node of
        LambdaExpression lambda ->
            case lambda.args of
                [ argPattern ] ->
                    case Node.value argPattern of
                        Pattern.VarPattern paramName ->
                            checkLambdaBody (Node.range node) paramName (Node.value lambda.expression)

                        _ ->
                            []

                _ ->
                    []

        _ ->
            []


{-| Try to extract a chain of function applications ending in a field access.
Returns (list of function names, field name) if successful.
-}
extractCompositionChain : String -> Expression -> Maybe ( List String, String )
extractCompositionChain paramName expr =
    case expr of
        Application [ funcNode, argNode ] ->
            case expressionToString (Node.value funcNode) of
                Just funcName ->
                    case extractArgument paramName (Node.value argNode) of
                        Just ( moreFuncs, fieldName ) ->
                            Just ( funcName :: moreFuncs, fieldName )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        _ ->
            Nothing


{-| Extract the argument, which could be a field access or another nested application.
-}
extractArgument : String -> Expression -> Maybe ( List String, String )
extractArgument paramName expr =
    case expr of
        -- Direct field access: x.field
        RecordAccess recordNode (Node _ fieldName) ->
            case Node.value recordNode of
                FunctionOrValue [] varName ->
                    if varName == paramName then
                        Just ( [], fieldName )

                    else
                        Nothing

                _ ->
                    Nothing

        -- Parenthesized expression: (g x.field) or (g (h x.field))
        ParenthesizedExpression innerNode ->
            extractCompositionChain paramName (Node.value innerNode)

        -- Nested application without parens (might happen)
        Application _ ->
            extractCompositionChain paramName expr

        _ ->
            Nothing


checkLambdaBody : Range -> String -> Expression -> List (Rule.Error {})
checkLambdaBody range paramName body =
    case extractCompositionChain paramName body of
        Just ( funcs, fieldName ) ->
            let
                composedFuncs =
                    String.join " << " funcs

                -- Don't wrap in parens - the lambda is typically already in parens
                replacement =
                    composedFuncs ++ " << ." ++ fieldName

                originalExpr =
                    buildOriginalExpr paramName funcs fieldName
            in
            [ Rule.errorWithFix
                { message = "Lambda can be simplified to function composition"
                , details =
                    [ "Instead of `(\\" ++ paramName ++ " -> " ++ originalExpr ++ ")`, you can write `(" ++ replacement ++ ")`."
                    ]
                }
                range
                [ Fix.replaceRangeBy range replacement ]
            ]

        Nothing ->
            []


{-| Build a string representation of the original expression for the error message.
-}
buildOriginalExpr : String -> List String -> String -> String
buildOriginalExpr paramName funcs fieldName =
    case funcs of
        [] ->
            paramName ++ "." ++ fieldName

        [ f ] ->
            f ++ " " ++ paramName ++ "." ++ fieldName

        f :: rest ->
            f ++ " (" ++ buildOriginalExpr paramName rest fieldName ++ ")"


expressionToString : Expression -> Maybe String
expressionToString expr =
    case expr of
        FunctionOrValue moduleName name ->
            case moduleName of
                [] ->
                    Just name

                _ ->
                    Just (String.join "." moduleName ++ "." ++ name)

        _ ->
            Nothing
