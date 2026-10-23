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
    Rule.newModuleRuleSchema "SimplifyLambdaToComposition" initialContext
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { comments : List Range
    , operandsNeedingParens : List Range
    }


initialContext : Context
initialContext =
    { comments = []
    , operandsNeedingParens = []
    }


commentsVisitor : List (Node String) -> Context -> ( List (Rule.Error {}), Context )
commentsVisitor comments context =
    ( [], { context | comments = List.map Node.range comments } )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        OperatorApplication operator _ _ right ->
            -- A lambda can only be an operator's right operand without parentheses. After
            -- `<|`, `|>` and `<<` a composition parses the same way; after any other
            -- operator (e.g. `>>`, which can't be mixed with `<<`) it needs parentheses.
            if List.member operator [ "<|", "|>", "<<" ] then
                ( [], context )

            else
                ( [], { context | operandsNeedingParens = Node.range right :: context.operandsNeedingParens } )

        LambdaExpression lambda ->
            case lambda.args of
                [ Node _ (Pattern.VarPattern paramName) ] ->
                    ( checkLambdaBody context (Node.range node) paramName (Node.value lambda.expression), context )

                _ ->
                    ( [], context )

        _ ->
            ( [], context )


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


checkLambdaBody : Context -> Range -> String -> Expression -> List (Rule.Error {})
checkLambdaBody context range paramName body =
    case extractCompositionChain paramName body of
        Just ( funcs, fieldName ) ->
            if List.member paramName funcs then
                -- `\f -> f f.field`: the function is the parameter itself
                []

            else
                reportComposition context range paramName funcs fieldName

        Nothing ->
            []


reportComposition : Context -> Range -> String -> List String -> String -> List (Rule.Error {})
reportComposition context range paramName funcs fieldName =
    let
        replacement : String
        replacement =
            String.join " << " funcs ++ " << ." ++ fieldName

        error : { message : String, details : List String }
        error =
            { message = "Lambda can be simplified to function composition"
            , details =
                [ "Instead of `(\\" ++ paramName ++ " -> " ++ buildOriginalExpr paramName funcs fieldName ++ ")`, you can write `(" ++ replacement ++ ")`."
                ]
            }
    in
    if List.any (isWithin range) context.comments then
        -- The fix would drop the comment
        [ Rule.error error range ]

    else if List.member range context.operandsNeedingParens then
        [ Rule.errorWithFix error range [ Fix.replaceRangeBy range ("(" ++ replacement ++ ")") ] ]

    else
        [ Rule.errorWithFix error range [ Fix.replaceRangeBy range replacement ] ]


isWithin : Range -> Range -> Bool
isWithin outer inner =
    ( outer.start.row, outer.start.column )
        <= ( inner.start.row, inner.start.column )
        && ( inner.end.row, inner.end.column )
        <= ( outer.end.row, outer.end.column )


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
