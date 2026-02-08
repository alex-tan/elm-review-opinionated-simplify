module PreferMaybeChaining exposing (rule)

{-| Prefer Maybe.map, Maybe.andThen, and Maybe.withDefault over case expressions.

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports case expressions on Maybe that can be simplified.

    config =
        [ PreferMaybeChaining.rule
        ]


## Fail

    case maybeValue of
        Nothing ->
            Nothing

        Just x ->
            Just (f x)

    case maybeValue of
        Nothing ->
            Nothing

        Just x ->
            lookup x

    case maybeValue of
        Nothing ->
            defaultValue

        Just x ->
            x


## Success

    Maybe.map f maybeValue

    Maybe.andThen lookup maybeValue

    Maybe.withDefault defaultValue maybeValue

    Maybe.Extra.withDefaultLazy (\() -> complexExpr) maybeValue


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template alex-tan/elm-review-opinionated-simplify/example --rules PreferMaybeChaining
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "PreferMaybeChaining" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor node =
    case Node.value node of
        CaseExpression caseExpr ->
            checkCaseExpression (Node.range node) caseExpr

        _ ->
            []


checkCaseExpression : Range -> Expression.CaseBlock -> List (Rule.Error {})
checkCaseExpression range caseBlock =
    case caseBlock.cases of
        [ first, second ] ->
            case extractMaybePattern caseBlock.expression first second of
                Just info ->
                    createError range info

                Nothing ->
                    []

        _ ->
            []


type MaybePatternInfo
    = NothingToNothing
        { boundPattern : String
        , justExpr : Expression
        , caseSubject : String
        }
    | NothingToDefault
        { boundPattern : String
        , defaultExpr : Expression
        , caseSubject : String
        }


{-| Try to extract a Maybe case pattern
-}
extractMaybePattern : Node Expression -> Expression.Case -> Expression.Case -> Maybe MaybePatternInfo
extractMaybePattern subject first second =
    -- Try both orderings: Nothing first or Just first
    case tryExtract subject first second of
        Just info ->
            Just info

        Nothing ->
            tryExtract subject second first


tryExtract : Node Expression -> Expression.Case -> Expression.Case -> Maybe MaybePatternInfo
tryExtract subject nothingCase justCase =
    -- Check if first is Nothing -> something
    case ( Node.value (Tuple.first nothingCase), Node.value (Tuple.second nothingCase) ) of
        ( Pattern.NamedPattern { name } [], nothingExpr ) ->
            if name == "Nothing" then
                -- Check if second is Just pattern -> expr
                case Node.value (Tuple.first justCase) of
                    Pattern.NamedPattern justPattern [ boundPatternNode ] ->
                        if justPattern.name == "Just" then
                            case patternToString (Node.value boundPatternNode) of
                                Just patternStr ->
                                    let
                                        justExpr =
                                            Node.value (Tuple.second justCase)
                                    in
                                    if isNothingExpression nothingExpr then
                                        -- Nothing -> Nothing pattern
                                        Just
                                            (NothingToNothing
                                                { boundPattern = patternStr
                                                , justExpr = justExpr
                                                , caseSubject = nodeToString subject
                                                }
                                            )

                                    else if isJustBoundVariable patternStr justExpr then
                                        -- Just x -> x pattern (withDefault)
                                        Just
                                            (NothingToDefault
                                                { boundPattern = patternStr
                                                , defaultExpr = nothingExpr
                                                , caseSubject = nodeToString subject
                                                }
                                            )

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing

                        else
                            Nothing

                    _ ->
                        Nothing

            else
                Nothing

        _ ->
            Nothing


{-| Check if the Just branch just returns the bound variable
-}
isJustBoundVariable : String -> Expression -> Bool
isJustBoundVariable boundVar expr =
    case expr of
        FunctionOrValue [] name ->
            name == boundVar

        ParenthesizedExpression innerNode ->
            isJustBoundVariable boundVar (Node.value innerNode)

        _ ->
            False


{-| Convert a pattern to its string representation
-}
patternToString : Pattern -> Maybe String
patternToString pattern =
    case pattern of
        Pattern.VarPattern name ->
            Just name

        Pattern.TuplePattern nodes ->
            let
                parts =
                    List.map (Node.value >> patternToString) nodes
            in
            if List.all ((/=) Nothing) parts then
                Just ("( " ++ String.join ", " (List.filterMap identity parts) ++ " )")

            else
                Nothing

        Pattern.RecordPattern fields ->
            Just ("{ " ++ String.join ", " (List.map Node.value fields) ++ " }")

        Pattern.AllPattern ->
            Just "_"

        Pattern.NamedPattern { moduleName, name } args ->
            let
                argStrings =
                    List.map (Node.value >> patternToString) args
            in
            if List.all ((/=) Nothing) argStrings then
                let
                    qualifiedName =
                        if List.isEmpty moduleName then
                            name

                        else
                            String.join "." moduleName ++ "." ++ name
                in
                if List.isEmpty args then
                    Just qualifiedName

                else
                    Just (qualifiedName ++ " " ++ String.join " " (List.filterMap identity argStrings))

            else
                Nothing

        Pattern.ParenthesizedPattern innerNode ->
            patternToString (Node.value innerNode)
                |> Maybe.map (\s -> "(" ++ s ++ ")")

        Pattern.AsPattern innerNode (Node _ alias) ->
            patternToString (Node.value innerNode)
                |> Maybe.map (\s -> s ++ " as " ++ alias)

        Pattern.UnConsPattern headNode tailNode ->
            Maybe.map2
                (\h t -> h ++ " :: " ++ t)
                (patternToString (Node.value headNode))
                (patternToString (Node.value tailNode))

        Pattern.ListPattern nodes ->
            let
                parts =
                    List.map (Node.value >> patternToString) nodes
            in
            if List.all ((/=) Nothing) parts then
                Just ("[" ++ String.join ", " (List.filterMap identity parts) ++ "]")

            else
                Nothing

        Pattern.IntPattern n ->
            Just (String.fromInt n)

        Pattern.StringPattern s ->
            Just ("\"" ++ s ++ "\"")

        Pattern.CharPattern c ->
            Just ("'" ++ String.fromChar c ++ "'")

        Pattern.FloatPattern f ->
            Just (String.fromFloat f)

        Pattern.HexPattern n ->
            Just (String.fromInt n)

        Pattern.UnitPattern ->
            Just "()"


isNothingExpression : Expression -> Bool
isNothingExpression expr =
    case expr of
        FunctionOrValue _ name ->
            name == "Nothing"

        _ ->
            False


createError : Range -> MaybePatternInfo -> List (Rule.Error {})
createError range patternInfo =
    let
        -- Use the starting column of the case expression for indentation
        indent =
            String.repeat (range.start.column - 1) " "

        -- Pipe is indented one level further
        pipeIndent =
            indent ++ "    "
    in
    case patternInfo of
        NothingToNothing info ->
            case classifyJustBranch info.justExpr of
                MaybeMapCase innerExpr ->
                    let
                        -- Lambda body indentation (one more level)
                        lambdaBodyIndent =
                            pipeIndent ++ "    "

                        innerExprStr =
                            expressionToStringWithIndent lambdaBodyIndent innerExpr

                        replacement =
                            info.caseSubject ++ "\n" ++ pipeIndent ++ "|> Maybe.map (\\" ++ info.boundPattern ++ " -> " ++ innerExprStr ++ ")"
                    in
                    if String.contains "<expr>" replacement then
                        -- Can't fully serialize the expression, report error without fix
                        [ Rule.error
                            { message = "Use Maybe.map instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.map with a pipe." ]
                            }
                            range
                        ]

                    else
                        [ Rule.errorWithFix
                            { message = "Use Maybe.map instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.map with a pipe." ]
                            }
                            range
                            [ Fix.replaceRangeBy range replacement ]
                        ]

                MaybeAndThenCase innerExpr segments ->
                    let
                        -- Format the first expression (goes in the lambda body)
                        firstExprStr =
                            expressionToString innerExpr

                        -- Format the additional segments as flattened pipes
                        -- Each segment's argument needs to know its base indentation
                        segmentsStr =
                            segments
                                |> List.map
                                    (\seg ->
                                        if seg.operator == "COMPLEX" then
                                            -- Can't flatten, keep as single expression
                                            "\n" ++ pipeIndent ++ "|> " ++ expressionToStringWithIndent pipeIndent seg.argument

                                        else
                                            "\n" ++ pipeIndent ++ "|> " ++ seg.operator ++ " " ++ expressionToStringAsArgWithBaseIndent pipeIndent seg.argument
                                    )
                                |> String.concat

                        replacement =
                            info.caseSubject ++ "\n" ++ pipeIndent ++ "|> Maybe.andThen (\\" ++ info.boundPattern ++ " -> " ++ firstExprStr ++ ")" ++ segmentsStr
                    in
                    if String.contains "<expr>" replacement || String.contains "COMPLEX" replacement then
                        -- Can't fully serialize the expression, report error without fix
                        [ Rule.error
                            { message = "Use Maybe.andThen instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.andThen with a pipe." ]
                            }
                            range
                        ]

                    else
                        [ Rule.errorWithFix
                            { message = "Use Maybe.andThen instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.andThen with a pipe." ]
                            }
                            range
                            [ Fix.replaceRangeBy range replacement ]
                        ]

                NotApplicable ->
                    []

        NothingToDefault info ->
            case classifyDefaultExpr info.defaultExpr of
                SimpleDefault defaultStr ->
                    let
                        replacement =
                            info.caseSubject ++ "\n" ++ pipeIndent ++ "|> Maybe.withDefault " ++ defaultStr
                    in
                    [ Rule.errorWithFix
                        { message = "Use Maybe.withDefault instead of case expression"
                        , details = [ "This case expression can be simplified using Maybe.withDefault." ]
                        }
                        range
                        [ Fix.replaceRangeBy range replacement ]
                    ]

                ComplexDefault defaultStr ->
                    let
                        replacement =
                            info.caseSubject ++ "\n" ++ pipeIndent ++ "|> Maybe.Extra.withDefaultLazy (\\() -> " ++ defaultStr ++ ")"
                    in
                    if String.contains "<expr>" replacement then
                        [ Rule.error
                            { message = "Use Maybe.Extra.withDefaultLazy instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.Extra.withDefaultLazy." ]
                            }
                            range
                        ]

                    else
                        [ Rule.errorWithFix
                            { message = "Use Maybe.Extra.withDefaultLazy instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.Extra.withDefaultLazy." ]
                            }
                            range
                            [ Fix.replaceRangeBy range replacement ]
                        ]


type DefaultExprType
    = SimpleDefault String
    | ComplexDefault String


{-| Classify whether the default expression is simple (variable/literal) or complex
-}
classifyDefaultExpr : Expression -> DefaultExprType
classifyDefaultExpr expr =
    case expr of
        -- Simple local variable
        FunctionOrValue [] name ->
            SimpleDefault name

        -- Qualified variable (e.g., Module.value)
        FunctionOrValue moduleName name ->
            SimpleDefault (String.join "." moduleName ++ "." ++ name)

        -- Literals
        Integer n ->
            SimpleDefault (String.fromInt n)

        Floatable n ->
            SimpleDefault (String.fromFloat n)

        Literal s ->
            SimpleDefault ("\"" ++ s ++ "\"")

        CharLiteral c ->
            SimpleDefault ("'" ++ String.fromChar c ++ "'")

        -- Empty list
        ListExpr [] ->
            SimpleDefault "[]"

        -- Unit
        UnitExpr ->
            SimpleDefault "()"

        -- Parenthesized simple expression
        ParenthesizedExpression innerNode ->
            case classifyDefaultExpr (Node.value innerNode) of
                SimpleDefault str ->
                    SimpleDefault str

                ComplexDefault str ->
                    ComplexDefault str

        -- Everything else is complex
        _ ->
            ComplexDefault (expressionToString expr)


type JustBranchType
    = MaybeMapCase Expression
    | MaybeAndThenCase Expression (List PipeSegment)
    | NotApplicable


{-| A segment of a pipe chain, like `|> Maybe.andThen f` or `|> Maybe.map g`
-}
type alias PipeSegment =
    { operator : String -- e.g., "Maybe.andThen", "Maybe.map"
    , argument : Expression -- the function argument
    }


{-| Extract pipe segments from a pipe expression.
Returns (firstExpr, segments) where firstExpr is the leftmost expression
and segments are the pipe operations.
-}
extractPipeSegments : Expression -> ( Expression, List PipeSegment )
extractPipeSegments expr =
    case expr of
        OperatorApplication "|>" _ leftNode rightNode ->
            let
                ( firstExpr, leftSegments ) =
                    extractPipeSegments (Node.value leftNode)

                rightExpr =
                    Node.value rightNode
            in
            case rightExpr of
                -- |> Maybe.andThen f or |> Maybe.map f
                Application ((Node _ (FunctionOrValue moduleName funcName)) :: args) ->
                    if (List.isEmpty moduleName || moduleName == [ "Maybe" ] || moduleName == [ "Maybe", "Extra" ]) && List.length args == 1 then
                        let
                            qualifiedName =
                                if List.isEmpty moduleName then
                                    funcName

                                else
                                    String.join "." moduleName ++ "." ++ funcName

                            segment =
                                { operator = qualifiedName
                                , argument = Node.value (List.head args |> Maybe.withDefault (Node.empty UnitExpr))
                                }
                        in
                        ( firstExpr, leftSegments ++ [ segment ] )

                    else
                        -- Not a Maybe operation, keep as single segment
                        ( firstExpr, leftSegments ++ [ { operator = "COMPLEX", argument = rightExpr } ] )

                _ ->
                    -- Not a function application
                    ( firstExpr, leftSegments ++ [ { operator = "COMPLEX", argument = rightExpr } ] )

        _ ->
            ( expr, [] )


classifyJustBranch : Expression -> JustBranchType
classifyJustBranch expr =
    case expr of
        -- Just x -> Maybe.map with identity (single variable)
        FunctionOrValue [] _ ->
            -- Could be identity or a function returning Maybe - treat as andThen to be safe
            MaybeAndThenCase expr []

        -- Just (something) or Just something -> Maybe.map
        Application ((Node _ (FunctionOrValue moduleName "Just")) :: args) ->
            if List.isEmpty moduleName || moduleName == [ "Maybe" ] then
                case args of
                    [ argNode ] ->
                        MaybeMapCase (Node.value argNode)

                    _ ->
                        NotApplicable

            else
                -- Not Just, it's a function call returning Maybe -> andThen
                MaybeAndThenCase expr []

        -- Any other application (not starting with Just) -> andThen
        Application _ ->
            MaybeAndThenCase expr []

        -- Record access like r.name without Just wrapper -> andThen
        RecordAccess _ _ ->
            MaybeAndThenCase expr []

        -- If expression -> andThen (since it can return Nothing)
        IfBlock _ _ _ ->
            MaybeAndThenCase expr []

        -- Let expression -> check what it returns, but keep full expression for formatting
        LetExpression letBlock ->
            case classifyJustBranch (Node.value letBlock.expression) of
                MaybeMapCase _ ->
                    -- Let returns Just something -> still treat the whole let as andThen
                    MaybeAndThenCase expr []

                MaybeAndThenCase _ _ ->
                    MaybeAndThenCase expr []

                NotApplicable ->
                    NotApplicable

        -- Parenthesized expression -> unwrap and check, but keep parens in output
        ParenthesizedExpression innerNode ->
            case classifyJustBranch (Node.value innerNode) of
                MaybeMapCase innerExpr ->
                    MaybeMapCase innerExpr

                MaybeAndThenCase _ _ ->
                    MaybeAndThenCase expr []

                NotApplicable ->
                    NotApplicable

        -- Pipe expression -> extract and flatten the pipe chain
        OperatorApplication "|>" _ _ _ ->
            let
                ( firstExpr, segments ) =
                    extractPipeSegments expr
            in
            MaybeAndThenCase firstExpr segments

        _ ->
            NotApplicable


nodeToString : Node Expression -> String
nodeToString node =
    expressionToString (Node.value node)


{-| Check if an expression needs parentheses when used as an argument
-}
needsParens : Expression -> Bool
needsParens expr =
    case expr of
        Application (_ :: _ :: _) ->
            -- Multi-argument application needs parens
            True

        IfBlock _ _ _ ->
            True

        OperatorApplication _ _ _ _ ->
            True

        _ ->
            False


expressionToString : Expression -> String
expressionToString =
    expressionToStringWithIndent ""


expressionToStringAsArgWithBaseIndent : String -> Expression -> String
expressionToStringAsArgWithBaseIndent baseIndent expr =
    case expr of
        -- Parenthesized expressions - keep the parens since they were in source
        ParenthesizedExpression innerNode ->
            "(" ++ expressionToStringWithIndent baseIndent (Node.value innerNode) ++ ")"

        _ ->
            if needsParens expr then
                "(" ++ expressionToStringWithIndent baseIndent expr ++ ")"

            else
                expressionToStringWithIndent baseIndent expr


{-| Convert expression to string with proper indentation for pipe expressions.
The baseIndent is the indentation to use for continuation lines (pipes).
-}
expressionToStringWithIndent : String -> Expression -> String
expressionToStringWithIndent baseIndent expr =
    let
        nextIndent =
            baseIndent ++ "    "
    in
    case expr of
        FunctionOrValue [] name ->
            name

        FunctionOrValue moduleName name ->
            String.join "." moduleName ++ "." ++ name

        Application nodes ->
            case nodes of
                [] ->
                    ""

                [ single ] ->
                    nodeToStringWithIndent baseIndent single

                funcNode :: argNodes ->
                    -- First element is the function, rest are arguments
                    nodeToStringWithIndent baseIndent funcNode ++ " " ++ String.join " " (List.map (nodeToStringAsArgWithIndent baseIndent) argNodes)

        RecordAccess recordNode (Node _ fieldName) ->
            nodeToStringWithIndent baseIndent recordNode ++ "." ++ fieldName

        RecordAccessFunction fieldName ->
            -- fieldName already includes the dot
            fieldName

        ParenthesizedExpression innerNode ->
            -- Don't add extra parens, just return the inner content
            nodeToStringWithIndent baseIndent innerNode

        IfBlock condNode thenNode elseNode ->
            "if " ++ nodeToStringWithIndent baseIndent condNode ++ " then " ++ nodeToStringWithIndent baseIndent thenNode ++ " else " ++ nodeToStringWithIndent baseIndent elseNode

        OperatorApplication "|>" _ leftNode rightNode ->
            -- Format pipe with newline
            nodeToStringWithIndent baseIndent leftNode ++ "\n" ++ nextIndent ++ "|> " ++ nodeToStringWithIndent nextIndent rightNode

        OperatorApplication op _ leftNode rightNode ->
            nodeToStringWithIndent baseIndent leftNode ++ " " ++ op ++ " " ++ nodeToStringWithIndent baseIndent rightNode

        Integer n ->
            String.fromInt n

        Floatable n ->
            String.fromFloat n

        Literal s ->
            "\"" ++ s ++ "\""

        ListExpr items ->
            "[" ++ String.join ", " (List.map (nodeToStringWithIndent baseIndent) items) ++ "]"

        TupledExpression items ->
            "( " ++ String.join ", " (List.map (nodeToStringWithIndent baseIndent) items) ++ " )"

        LambdaExpression lambda ->
            let
                argsStr =
                    List.filterMap (Node.value >> patternToString) lambda.args
                        |> String.join " "

                bodyStr =
                    nodeToStringWithIndent nextIndent lambda.expression
            in
            -- Check if body contains pipes/let and format accordingly
            if String.contains "\n" bodyStr then
                "(\\" ++ argsStr ++ " ->\n" ++ nextIndent ++ bodyStr ++ "\n" ++ baseIndent ++ ")"

            else
                "(\\" ++ argsStr ++ " -> " ++ bodyStr ++ ")"

        LetExpression letBlock ->
            let
                declarationsStr =
                    List.map (letDeclarationToStringWithIndent nextIndent) letBlock.declarations
                        |> String.join ("\n" ++ nextIndent)

                bodyStr =
                    nodeToStringWithIndent baseIndent letBlock.expression
            in
            "let\n" ++ nextIndent ++ declarationsStr ++ "\n" ++ baseIndent ++ "in\n" ++ baseIndent ++ bodyStr

        CaseExpression caseBlock ->
            let
                casesStr =
                    List.map (caseToStringWithIndent baseIndent) caseBlock.cases
                        |> String.join " ; "
            in
            "case " ++ nodeToStringWithIndent baseIndent caseBlock.expression ++ " of " ++ casesStr

        Negation innerNode ->
            "-" ++ nodeToStringWithIndent baseIndent innerNode

        CharLiteral c ->
            "'" ++ String.fromChar c ++ "'"

        RecordExpr fields ->
            let
                fieldStr ( Node _ fieldName, valueNode ) =
                    fieldName ++ " = " ++ nodeToStringWithIndent baseIndent valueNode
            in
            "{ " ++ String.join ", " (List.map (Node.value >> fieldStr) fields) ++ " }"

        RecordUpdateExpression (Node _ recordName) fields ->
            let
                fieldStr ( Node _ fieldName, valueNode ) =
                    fieldName ++ " = " ++ nodeToStringWithIndent baseIndent valueNode
            in
            "{ " ++ recordName ++ " | " ++ String.join ", " (List.map (Node.value >> fieldStr) fields) ++ " }"

        UnitExpr ->
            "()"

        _ ->
            "<expr>"


nodeToStringWithIndent : String -> Node Expression -> String
nodeToStringWithIndent baseIndent node =
    expressionToStringWithIndent baseIndent (Node.value node)


nodeToStringAsArgWithIndent : String -> Node Expression -> String
nodeToStringAsArgWithIndent baseIndent node =
    let
        expr =
            Node.value node
    in
    case expr of
        -- Parenthesized expressions - keep the parens since they were in source
        ParenthesizedExpression innerNode ->
            "(" ++ expressionToStringWithIndent baseIndent (Node.value innerNode) ++ ")"

        _ ->
            if needsParens expr then
                "(" ++ expressionToStringWithIndent baseIndent expr ++ ")"

            else
                expressionToStringWithIndent baseIndent expr


letDeclarationToStringWithIndent : String -> Node Expression.LetDeclaration -> String
letDeclarationToStringWithIndent baseIndent node =
    let
        nextIndent =
            baseIndent ++ "    "
    in
    case Node.value node of
        Expression.LetFunction func ->
            let
                declaration =
                    Node.value func.declaration

                name =
                    Node.value declaration.name

                argsStr =
                    List.filterMap (Node.value >> patternToString) declaration.arguments
                        |> String.join " "

                argsWithSpace =
                    if String.isEmpty argsStr then
                        ""

                    else
                        " " ++ argsStr

                valueStr =
                    nodeToStringWithIndent nextIndent declaration.expression
            in
            if String.contains "\n" valueStr then
                name ++ argsWithSpace ++ " =\n" ++ nextIndent ++ valueStr

            else
                name ++ argsWithSpace ++ " = " ++ valueStr

        Expression.LetDestructuring pattern expr ->
            case patternToString (Node.value pattern) of
                Just patStr ->
                    let
                        valueStr =
                            nodeToStringWithIndent nextIndent expr
                    in
                    if String.contains "\n" valueStr then
                        patStr ++ " =\n" ++ nextIndent ++ valueStr

                    else
                        patStr ++ " = " ++ valueStr

                Nothing ->
                    "<decl>"


caseToStringWithIndent : String -> Expression.Case -> String
caseToStringWithIndent baseIndent ( patternNode, exprNode ) =
    case patternToString (Node.value patternNode) of
        Just patStr ->
            patStr ++ " -> " ++ nodeToStringWithIndent baseIndent exprNode

        Nothing ->
            "<case>"
