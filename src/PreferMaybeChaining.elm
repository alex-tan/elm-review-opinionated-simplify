module PreferMaybeChaining exposing (rule)

{-| Prefer Maybe.map, Maybe.andThen, and Maybe.withDefault over case expressions.

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import Review.Fix as Fix exposing (Fix)
import Review.Project.Dependency exposing (Dependency)
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

A case expression whose `Just` or `Nothing` branch makes a tail call to the function
it is in is not reported, since moving that call into a lambda would lose tail-call
optimisation.

The `Maybe.Extra.withDefaultLazy` fix is only offered when `elm-community/maybe-extra` is a
direct dependency of the project.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template alex-tan/elm-review-opinionated-simplify/example --rules PreferMaybeChaining
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "PreferMaybeChaining" { hasMaybeExtra = False }
        |> Rule.withDirectDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = initialContext
            , fromModuleToProject = Rule.initContextCreator (\context -> { hasMaybeExtra = context.hasMaybeExtra })
            , foldProjectContexts = \new _ -> new
            }
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} Context -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } Context
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor



-- CONTEXT


type alias ProjectContext =
    { hasMaybeExtra : Bool
    }


type alias Context =
    { extractSourceCode : Range -> String
    , comments : List Range

    -- How `Maybe.Extra` is referred to in this module, if it is imported
    , maybeExtraQualifier : Maybe String

    -- Whether elm-community/maybe-extra is a direct dependency
    , hasMaybeExtra : Bool

    -- Where to add `import Maybe.Extra` when it isn't imported
    , importLocation : Location

    -- Right operands of operators: a pipeline there needs parentheses
    , operatorOperands : List Range

    -- The functions (top-level and let) we might be inside, with their ranges
    , enclosingFunctions : List ( Range, String )
    }


initialContext : Rule.ContextCreator ProjectContext Context
initialContext =
    Rule.initContextCreator
        (\extractSourceCode projectContext ->
            { extractSourceCode = extractSourceCode
            , comments = []
            , maybeExtraQualifier = Nothing
            , hasMaybeExtra = projectContext.hasMaybeExtra
            , importLocation = { row = 1, column = 1 }
            , operatorOperands = []
            , enclosingFunctions = []
            }
        )
        |> Rule.withSourceCodeExtractor


moduleDefinitionVisitor : Node Module -> Context -> ( List (Rule.Error {}), Context )
moduleDefinitionVisitor node context =
    ( [], { context | importLocation = (Node.range node).end } )


commentsVisitor : List (Node String) -> Context -> ( List (Rule.Error {}), Context )
commentsVisitor comments context =
    let
        -- Imports go after the module documentation, the first `{-|` comment
        -- (declarations' documentation isn't part of the comments)
        importLocation : Location
        importLocation =
            comments
                |> List.filter (Node.value >> String.startsWith "{-|")
                |> List.head
                |> Maybe.map (Node.range >> .end)
                |> Maybe.withDefault context.importLocation
    in
    ( []
    , { context
        | comments = List.map Node.range comments
        , importLocation = importLocation
      }
    )


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List (Rule.Error { useErrorForModule : () }), ProjectContext )
dependenciesVisitor dependencies _ =
    ( [], { hasMaybeExtra = Dict.member "elm-community/maybe-extra" dependencies } )


importVisitor : Node Import -> Context -> ( List (Rule.Error {}), Context )
importVisitor node context =
    let
        import_ : Import
        import_ =
            Node.value node

        maybeExtraQualifier : Maybe String
        maybeExtraQualifier =
            if Node.value import_.moduleName == [ "Maybe", "Extra" ] then
                import_.moduleAlias
                    |> Maybe.map (Node.value >> String.join ".")
                    |> Maybe.withDefault "Maybe.Extra"
                    |> Just

            else
                context.maybeExtraQualifier
    in
    ( []
    , { context
        | importLocation = (Node.range node).end
        , maybeExtraQualifier = maybeExtraQualifier
      }
    )


declarationVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            ( []
            , { context
                | enclosingFunctions = [ ( Node.range node, Node.value (Node.value function.declaration).name ) ]
              }
            )

        _ ->
            ( [], { context | enclosingFunctions = [] } )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        OperatorApplication _ _ _ right ->
            ( [], { context | operatorOperands = Node.range right :: context.operatorOperands } )

        LetExpression letBlock ->
            ( []
            , { context
                | enclosingFunctions = List.filterMap letFunction letBlock.declarations ++ context.enclosingFunctions
              }
            )

        CaseExpression caseBlock ->
            ( checkCaseExpression context (Node.range node) caseBlock, context )

        _ ->
            ( [], context )


letFunction : Node Expression.LetDeclaration -> Maybe ( Range, String )
letFunction node =
    case Node.value node of
        Expression.LetFunction function ->
            Just ( Node.range node, Node.value (Node.value function.declaration).name )

        Expression.LetDestructuring _ _ ->
            Nothing



-- MATCHING


type alias MaybeCase =
    { subject : Node Expression
    , justPattern : Node Pattern
    , justExpr : Node Expression
    , nothingExpr : Node Expression
    }


checkCaseExpression : Context -> Range -> Expression.CaseBlock -> List (Rule.Error {})
checkCaseExpression context range caseBlock =
    case caseBlock.cases of
        [ first, second ] ->
            case extractMaybeCase caseBlock.expression first second of
                Just maybeCase ->
                    if makesTailCall (enclosingFunctionNames context range) maybeCase.justExpr || makesTailCall (enclosingFunctionNames context range) maybeCase.nothingExpr then
                        []

                    else
                        createError context range maybeCase

                Nothing ->
                    []

        _ ->
            []


{-| Names of the functions whose body contains the range.
-}
enclosingFunctionNames : Context -> Range -> List String
enclosingFunctionNames context range =
    context.enclosingFunctions
        |> List.filter (\( functionRange, _ ) -> isWithin functionRange range)
        |> List.map Tuple.second


extractMaybeCase : Node Expression -> Expression.Case -> Expression.Case -> Maybe MaybeCase
extractMaybeCase subject first second =
    -- Try both orderings: Nothing first or Just first
    case tryExtract subject first second of
        Just maybeCase ->
            Just maybeCase

        Nothing ->
            tryExtract subject second first


tryExtract : Node Expression -> Expression.Case -> Expression.Case -> Maybe MaybeCase
tryExtract subject ( nothingPattern, nothingExpr ) ( justPattern, justExpr ) =
    case ( Node.value nothingPattern, Node.value justPattern ) of
        ( Pattern.NamedPattern nothing [], Pattern.NamedPattern just [ boundPattern ] ) ->
            if nothing.name == "Nothing" && isMaybeModule nothing.moduleName && just.name == "Just" && isMaybeModule just.moduleName then
                Just
                    { subject = subject
                    , justPattern = boundPattern
                    , justExpr = justExpr
                    , nothingExpr = nothingExpr
                    }

            else
                Nothing

        _ ->
            Nothing


isMaybeModule : ModuleName -> Bool
isMaybeModule moduleName =
    List.isEmpty moduleName || moduleName == [ "Maybe" ]


isNothingExpression : Node Expression -> Bool
isNothingExpression node =
    case Node.value node of
        FunctionOrValue moduleName "Nothing" ->
            isMaybeModule moduleName

        _ ->
            False


{-| Check if the Just branch just returns the bound variable
-}
isBoundVariable : Node Pattern -> Node Expression -> Bool
isBoundVariable pattern expr =
    case ( Node.value pattern, Node.value (unwrapParens expr) ) of
        ( Pattern.VarPattern boundName, FunctionOrValue [] name ) ->
            name == boundName

        _ ->
            False


type JustBranch
    = MapTo (Node Expression)
    | AndThenTo (Node Expression) (List (Node Expression))
    | NotApplicable


classifyJustBranch : List String -> Node Expression -> JustBranch
classifyJustBranch boundNames node =
    case Node.value node of
        -- Just something -> Maybe.map
        Application ((Node _ (FunctionOrValue moduleName "Just")) :: args) ->
            if isMaybeModule moduleName then
                case args of
                    [ arg ] ->
                        MapTo (unwrapParens arg)

                    _ ->
                        NotApplicable

            else
                AndThenTo node []

        -- A variable could be a Maybe -> andThen
        FunctionOrValue [] _ ->
            AndThenTo node []

        -- A function call, a field or an if returning a Maybe -> andThen
        Application _ ->
            AndThenTo node []

        RecordAccess _ _ ->
            AndThenTo node []

        IfBlock _ _ _ ->
            AndThenTo node []

        LetExpression letBlock ->
            case classifyJustBranch boundNames letBlock.expression of
                NotApplicable ->
                    NotApplicable

                _ ->
                    AndThenTo node []

        ParenthesizedExpression inner ->
            case classifyJustBranch boundNames inner of
                MapTo mapped ->
                    MapTo mapped

                AndThenTo _ _ ->
                    AndThenTo node []

                NotApplicable ->
                    NotApplicable

        OperatorApplication "|>" _ _ _ ->
            let
                ( body, steps ) =
                    splitPipeline boundNames node
            in
            AndThenTo body steps

        _ ->
            NotApplicable


{-| Split `a |> s1 |> s2 |> s3` into the part that stays in the `Maybe.andThen` lambda and
the trailing steps that can move out after it. A step can move out only if it gives Nothing
for Nothing (`Maybe.map f` or `Maybe.andThen f`) and doesn't use a name the `Just` pattern binds.
-}
splitPipeline : List String -> Node Expression -> ( Node Expression, List (Node Expression) )
splitPipeline boundNames node =
    case Node.value node of
        OperatorApplication "|>" _ left right ->
            if isMovableStep boundNames right then
                let
                    ( body, steps ) =
                        splitPipeline boundNames left
                in
                ( body, steps ++ [ right ] )

            else
                ( node, [] )

        _ ->
            ( node, [] )


isMovableStep : List String -> Node Expression -> Bool
isMovableStep boundNames node =
    case Node.value node of
        Application [ Node _ (FunctionOrValue [ "Maybe" ] name), arg ] ->
            (name == "map" || name == "andThen") && not (references boundNames arg)

        _ ->
            False


{-| Does the expression make a tail call to one of the functions?
-}
makesTailCall : List String -> Node Expression -> Bool
makesTailCall names node =
    case Node.value node of
        Application (function :: _) ->
            isCallTo names function

        OperatorApplication "<|" _ function _ ->
            isCallTo names function

        OperatorApplication "|>" _ _ function ->
            isCallTo names function

        ParenthesizedExpression inner ->
            makesTailCall names inner

        IfBlock _ thenBranch elseBranch ->
            makesTailCall names thenBranch || makesTailCall names elseBranch

        LetExpression letBlock ->
            makesTailCall names letBlock.expression

        CaseExpression caseBlock ->
            List.any (Tuple.second >> makesTailCall names) caseBlock.cases

        _ ->
            False


isCallTo : List String -> Node Expression -> Bool
isCallTo names node =
    case Node.value node of
        FunctionOrValue [] name ->
            List.member name names

        Application (function :: _) ->
            isCallTo names function

        ParenthesizedExpression inner ->
            isCallTo names inner

        _ ->
            False



-- FIXING


createError : Context -> Range -> MaybeCase -> List (Rule.Error {})
createError context range maybeCase =
    if isNothingExpression maybeCase.nothingExpr then
        case classifyJustBranch (patternNames maybeCase.justPattern) maybeCase.justExpr of
            MapTo body ->
                [ report context
                    range
                    { message = "Use Maybe.map instead of case expression"
                    , details = [ "This case expression can be simplified using Maybe.map with a pipe." ]
                    }
                    { subject = maybeCase.subject
                    , kept = [ Node.range maybeCase.justPattern, Node.range body ]
                    , steps = [ "Maybe.map " ++ lambda context maybeCase.justPattern body ]
                    , extraFixes = []
                    }
                ]

            AndThenTo body steps ->
                [ report context
                    range
                    { message = "Use Maybe.andThen instead of case expression"
                    , details = [ "This case expression can be simplified using Maybe.andThen with a pipe." ]
                    }
                    { subject = maybeCase.subject
                    , kept = Node.range maybeCase.justPattern :: Node.range body :: List.map Node.range steps
                    , steps =
                        ("Maybe.andThen " ++ lambda context maybeCase.justPattern body)
                            :: List.map (Node.range >> context.extractSourceCode) steps
                    , extraFixes = []
                    }
                ]

            NotApplicable ->
                []

    else if isBoundVariable maybeCase.justPattern maybeCase.justExpr then
        withDefaultErrors context range maybeCase

    else
        []


{-| `Nothing -> default; Just x -> x`
-}
withDefaultErrors : Context -> Range -> MaybeCase -> List (Rule.Error {})
withDefaultErrors context range maybeCase =
    let
        default : Node Expression
        default =
            unwrapParens maybeCase.nothingExpr

        defaultSource : String
        defaultSource =
            context.extractSourceCode (Node.range default)
    in
    if isSimpleDefault (Node.value default) then
        [ report context
            range
            { message = "Use Maybe.withDefault instead of case expression"
            , details = [ "This case expression can be simplified using Maybe.withDefault." ]
            }
            { subject = maybeCase.subject
            , kept = [ Node.range default ]
            , steps = [ "Maybe.withDefault " ++ defaultSource ]
            , extraFixes = []
            }
        ]

    else if not context.hasMaybeExtra then
        [ Rule.error
            { message = "Use Maybe.Extra.withDefaultLazy instead of case expression"
            , details =
                [ "This case expression can be simplified using Maybe.Extra.withDefaultLazy."
                , "Add elm-community/maybe-extra to the project's dependencies to get an automatic fix."
                ]
            }
            range
        ]

    else
        [ report context
            range
            { message = "Use Maybe.Extra.withDefaultLazy instead of case expression"
            , details = [ "This case expression can be simplified using Maybe.Extra.withDefaultLazy." ]
            }
            { subject = maybeCase.subject
            , kept = [ Node.range default ]
            , steps = [ Maybe.withDefault "Maybe.Extra" context.maybeExtraQualifier ++ ".withDefaultLazy (\\() -> " ++ defaultSource ++ ")" ]
            , extraFixes =
                case context.maybeExtraQualifier of
                    Just _ ->
                        []

                    Nothing ->
                        [ Fix.insertAt context.importLocation "\nimport Maybe.Extra" ]
            }
        ]


type alias Rewrite =
    { subject : Node Expression

    -- The other parts of the case expression the rewrite copies over
    , kept : List Range

    -- What goes after each `|>`
    , steps : List String
    , extraFixes : List Fix
    }


{-| Report the case expression, with a fix that turns it into `subject |> step1 |> step2 ...`.

If the case expression has a comment outside of the parts the fix copies over, the fix would
drop it, so the error has no fix.

-}
report : Context -> Range -> { message : String, details : List String } -> Rewrite -> Rule.Error {}
report context range error rewrite =
    let
        kept : List Range
        kept =
            Node.range rewrite.subject :: rewrite.kept

        dropsComment : Bool
        dropsComment =
            List.any
                (\comment -> isWithin range comment && not (List.any (\keptRange -> isWithin keptRange comment) kept))
                context.comments
    in
    if dropsComment then
        Rule.error error range

    else
        Rule.errorWithFix error range (Fix.replaceRangeBy range (pipeline context range rewrite) :: rewrite.extraFixes)


pipeline : Context -> Range -> Rewrite -> String
pipeline context range rewrite =
    let
        -- The pipe is indented one level further than the case expression
        pipeIndent : String
        pipeIndent =
            String.repeat (range.start.column - 1 + 4) " "

        source : String
        source =
            subjectToString context rewrite.subject
                ++ String.concat (List.map (\step -> "\n" ++ pipeIndent ++ "|> " ++ step) rewrite.steps)
    in
    if List.member range context.operatorOperands then
        -- `f <| case ...` or `x ++ case ...`: the pipeline would otherwise take the operator's
        -- left side as its subject (or not compile, for `<|`)
        "(" ++ source ++ ")"

    else
        source


subjectToString : Context -> Node Expression -> String
subjectToString context subject =
    let
        unwrapped : Node Expression
        unwrapped =
            unwrapParens subject

        source : String
        source =
            context.extractSourceCode (Node.range unwrapped)
    in
    case Node.value unwrapped of
        -- These would swallow the `|>` or (for `<|`) not compile next to it
        OperatorApplication "<|" _ _ _ ->
            "(" ++ source ++ ")"

        IfBlock _ _ _ ->
            "(" ++ source ++ ")"

        LetExpression _ ->
            "(" ++ source ++ ")"

        CaseExpression _ ->
            "(" ++ source ++ ")"

        LambdaExpression _ ->
            "(" ++ source ++ ")"

        _ ->
            source


lambda : Context -> Node Pattern -> Node Expression -> String
lambda context pattern body =
    "(\\" ++ context.extractSourceCode (Node.range pattern) ++ " -> " ++ context.extractSourceCode (Node.range body) ++ ")"


{-| Whether the default is cheap enough to evaluate eagerly with `Maybe.withDefault`
-}
isSimpleDefault : Expression -> Bool
isSimpleDefault expr =
    case expr of
        FunctionOrValue _ _ ->
            True

        Integer _ ->
            True

        Hex _ ->
            True

        Negation inner ->
            case Node.value inner of
                Integer _ ->
                    True

                Hex _ ->
                    True

                Floatable _ ->
                    True

                _ ->
                    False

        Floatable _ ->
            True

        Literal _ ->
            True

        CharLiteral _ ->
            True

        ListExpr [] ->
            True

        UnitExpr ->
            True

        _ ->
            False



-- HELPERS


unwrapParens : Node Expression -> Node Expression
unwrapParens node =
    case Node.value node of
        ParenthesizedExpression inner ->
            unwrapParens inner

        _ ->
            node


isWithin : Range -> Range -> Bool
isWithin outer inner =
    ( outer.start.row, outer.start.column )
        <= ( inner.start.row, inner.start.column )
        && ( inner.end.row, inner.end.column )
        <= ( outer.end.row, outer.end.column )


patternNames : Node Pattern -> List String
patternNames node =
    case Node.value node of
        Pattern.VarPattern name ->
            [ name ]

        Pattern.AsPattern inner (Node _ name) ->
            name :: patternNames inner

        Pattern.ParenthesizedPattern inner ->
            patternNames inner

        Pattern.TuplePattern patterns ->
            List.concatMap patternNames patterns

        Pattern.ListPattern patterns ->
            List.concatMap patternNames patterns

        Pattern.NamedPattern _ patterns ->
            List.concatMap patternNames patterns

        Pattern.UnConsPattern head tail ->
            patternNames head ++ patternNames tail

        Pattern.RecordPattern fields ->
            List.map Node.value fields

        _ ->
            []


{-| Does the expression refer to any of the (unqualified) names?
-}
references : List String -> Node Expression -> Bool
references names node =
    case Node.value node of
        FunctionOrValue [] name ->
            List.member name names

        RecordUpdateExpression (Node _ name) fields ->
            List.member name names || List.any (Node.value >> Tuple.second >> references names) fields

        expr ->
            List.any (references names) (subExpressions expr)


subExpressions : Expression -> List (Node Expression)
subExpressions expr =
    case expr of
        Application nodes ->
            nodes

        OperatorApplication _ _ left right ->
            [ left, right ]

        IfBlock condition thenBranch elseBranch ->
            [ condition, thenBranch, elseBranch ]

        Negation inner ->
            [ inner ]

        TupledExpression nodes ->
            nodes

        ParenthesizedExpression inner ->
            [ inner ]

        LetExpression letBlock ->
            letBlock.expression :: List.map letDeclarationExpression letBlock.declarations

        CaseExpression caseBlock ->
            caseBlock.expression :: List.map Tuple.second caseBlock.cases

        LambdaExpression lambda_ ->
            [ lambda_.expression ]

        RecordExpr fields ->
            List.map (Node.value >> Tuple.second) fields

        ListExpr nodes ->
            nodes

        RecordAccess record _ ->
            [ record ]

        _ ->
            []


letDeclarationExpression : Node Expression.LetDeclaration -> Node Expression
letDeclarationExpression node =
    case Node.value node of
        Expression.LetFunction function ->
            (Node.value function.declaration).expression

        Expression.LetDestructuring _ expr ->
            expr
