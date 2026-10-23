module PreferMaybeChainingTest exposing (all)

import PreferMaybeChaining exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "PreferMaybeChaining"
        [ shouldUseMaybeMapTests
        , shouldUseMaybeAndThenTests
        , shouldUseMaybeWithDefaultTests
        , shouldUseMaybeExtraWithDefaultLazyTests
        , shouldNotChangeTests
        , edgeCaseTests
        ]


shouldUseMaybeMapTests : Test
shouldUseMaybeMapTests =
    describe "should use Maybe.map"
        [ test "simple case with Just wrapper" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> Nothing
        Just x -> Just (f x)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.map instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.map with a pipe." ]
                            , under = "case maybeValue of\n        Nothing -> Nothing\n        Just x -> Just (f x)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.map (\\x -> f x)
"""
                        ]
        , test "case with Just wrapper - branches reversed" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Just x -> Just (f x)
        Nothing -> Nothing
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.map instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.map with a pipe." ]
                            , under = "case maybeValue of\n        Just x -> Just (f x)\n        Nothing -> Nothing"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.map (\\x -> f x)
"""
                        ]
        , test "case returning Just of the bound variable directly" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> Nothing
        Just x -> Just x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.map instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.map with a pipe." ]
                            , under = "case maybeValue of\n        Nothing -> Nothing\n        Just x -> Just x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.map (\\x -> x)
"""
                        ]
        , test "case with qualified module function" <|
            \() ->
                """module A exposing (..)
a =
    case maybeTime of
        Nothing -> Nothing
        Just t -> Just (Time.posixToMillis t)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.map instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.map with a pipe." ]
                            , under = "case maybeTime of\n        Nothing -> Nothing\n        Just t -> Just (Time.posixToMillis t)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeTime
        |> Maybe.map (\\t -> Time.posixToMillis t)
"""
                        ]
        , test "case with field access" <|
            \() ->
                """module A exposing (..)
a =
    case maybeRecord of
        Nothing -> Nothing
        Just r -> Just r.name
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.map instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.map with a pipe." ]
                            , under = "case maybeRecord of\n        Nothing -> Nothing\n        Just r -> Just r.name"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeRecord
        |> Maybe.map (\\r -> r.name)
"""
                        ]
        ]


shouldUseMaybeAndThenTests : Test
shouldUseMaybeAndThenTests =
    describe "should use Maybe.andThen"
        [ test "case returning Maybe without Just wrapper" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> Nothing
        Just x -> lookup x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.andThen instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.andThen with a pipe." ]
                            , under = "case maybeValue of\n        Nothing -> Nothing\n        Just x -> lookup x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.andThen (\\x -> lookup x)
"""
                        ]
        , test "case with qualified function returning Maybe" <|
            \() ->
                """module A exposing (..)
a =
    case maybeId of
        Nothing -> Nothing
        Just id -> Store.findById id store
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.andThen instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.andThen with a pipe." ]
                            , under = "case maybeId of\n        Nothing -> Nothing\n        Just id -> Store.findById id store"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeId
        |> Maybe.andThen (\\id -> Store.findById id store)
"""
                        ]
        , test "case with tuple pattern and if expression" <|
            \() ->
                """module A exposing (..)
a =
    case maybeTuple of
        Nothing -> Nothing
        Just ( x, y ) -> if x == z then Just y else Nothing
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.andThen instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.andThen with a pipe." ]
                            , under = "case maybeTuple of\n        Nothing -> Nothing\n        Just ( x, y ) -> if x == z then Just y else Nothing"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeTuple
        |> Maybe.andThen (\\( x, y ) -> if x == z then Just y else Nothing)
"""
                        ]
        , test "case with tuple pattern and simple expression" <|
            \() ->
                """module A exposing (..)
a =
    case maybeTuple of
        Nothing -> Nothing
        Just ( x, y ) -> Just y
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.map instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.map with a pipe." ]
                            , under = "case maybeTuple of\n        Nothing -> Nothing\n        Just ( x, y ) -> Just y"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeTuple
        |> Maybe.map (\\( x, y ) -> y)
"""
                        ]
        , test "case with pipe expression returning Maybe - flattens" <|
            \() ->
                """module A exposing (..)
a =
    case maybeId of
        Nothing -> Nothing
        Just id -> lookup id |> Maybe.andThen otherLookup
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.andThen instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.andThen with a pipe." ]
                            , under = "case maybeId of\n        Nothing -> Nothing\n        Just id -> lookup id |> Maybe.andThen otherLookup"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeId
        |> Maybe.andThen (\\id -> lookup id)
        |> Maybe.andThen otherLookup
"""
                        ]
        , test "case with record access function in pipe - flattens" <|
            \() ->
                """module A exposing (..)
a =
    case maybeId of
        Nothing -> Nothing
        Just id -> lookup id |> Maybe.map .name
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.andThen instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.andThen with a pipe." ]
                            , under = "case maybeId of\n        Nothing -> Nothing\n        Just id -> lookup id |> Maybe.map .name"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeId
        |> Maybe.andThen (\\id -> lookup id)
        |> Maybe.map .name
"""
                        ]
        , test "case with complex let expression - flattens pipes" <|
            \() ->
                """module A exposing (..)
a =
    case maybeId of
        Nothing -> Nothing
        Just id ->
            lookup id
                |> Maybe.map
                    (\\item ->
                        let
                            detail = item.name
                        in
                        detail
                    )
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.andThen instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.andThen with a pipe." ]
                            , under = """case maybeId of
        Nothing -> Nothing
        Just id ->
            lookup id
                |> Maybe.map
                    (\\item ->
                        let
                            detail = item.name
                        in
                        detail
                    )"""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeId
        |> Maybe.andThen (\\id -> lookup id)
        |> Maybe.map
                    (\\item ->
                        let
                            detail = item.name
                        in
                        detail
                    )
"""
                        ]
        ]


shouldUseMaybeWithDefaultTests : Test
shouldUseMaybeWithDefaultTests =
    describe "should use Maybe.withDefault"
        [ test "simple variable default" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> defaultValue
        Just x -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.withDefault instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.withDefault." ]
                            , under = "case maybeValue of\n        Nothing -> defaultValue\n        Just x -> x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.withDefault defaultValue
"""
                        ]
        , test "qualified variable default" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> Config.defaultName
        Just x -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.withDefault instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.withDefault." ]
                            , under = "case maybeValue of\n        Nothing -> Config.defaultName\n        Just x -> x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.withDefault Config.defaultName
"""
                        ]
        , test "integer literal default" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> 0
        Just x -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.withDefault instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.withDefault." ]
                            , under = "case maybeValue of\n        Nothing -> 0\n        Just x -> x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.withDefault 0
"""
                        ]
        , test "string literal default" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> ""
        Just x -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.withDefault instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.withDefault." ]
                            , under = "case maybeValue of\n        Nothing -> \"\"\n        Just x -> x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.withDefault ""
"""
                        ]
        , test "empty list default" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> []
        Just x -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.withDefault instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.withDefault." ]
                            , under = "case maybeValue of\n        Nothing -> []\n        Just x -> x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.withDefault []
"""
                        ]
        , test "branches reversed - Just first" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Just x -> x
        Nothing -> defaultValue
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.withDefault instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.withDefault." ]
                            , under = "case maybeValue of\n        Just x -> x\n        Nothing -> defaultValue"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.withDefault defaultValue
"""
                        ]
        ]


shouldUseMaybeExtraWithDefaultLazyTests : Test
shouldUseMaybeExtraWithDefaultLazyTests =
    describe "should use Maybe.Extra.withDefaultLazy"
        [ test "function call default" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> computeDefault model
        Just x -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.Extra.withDefaultLazy instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.Extra.withDefaultLazy." ]
                            , under = "case maybeValue of\n        Nothing -> computeDefault model\n        Just x -> x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Maybe.Extra
a =
    maybeValue
        |> Maybe.Extra.withDefaultLazy (\\() -> computeDefault model)
"""
                        ]
        , test "record access default" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> model.defaultName
        Just x -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.Extra.withDefaultLazy instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.Extra.withDefaultLazy." ]
                            , under = "case maybeValue of\n        Nothing -> model.defaultName\n        Just x -> x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Maybe.Extra
a =
    maybeValue
        |> Maybe.Extra.withDefaultLazy (\\() -> model.defaultName)
"""
                        ]
        , test "if expression default" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> if flag then x else y
        Just v -> v
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.Extra.withDefaultLazy instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.Extra.withDefaultLazy." ]
                            , under = "case maybeValue of\n        Nothing -> if flag then x else y\n        Just v -> v"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Maybe.Extra
a =
    maybeValue
        |> Maybe.Extra.withDefaultLazy (\\() -> if flag then x else y)
"""
                        ]
        , test "nested function calls with parens preserved" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> Maybe.withDefault (Maybe.withDefault empty autoValue) savedValue
        Just v -> v
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Maybe.Extra.withDefaultLazy instead of case expression"
                            , details = [ "This case expression can be simplified using Maybe.Extra.withDefaultLazy." ]
                            , under = "case maybeValue of\n        Nothing -> Maybe.withDefault (Maybe.withDefault empty autoValue) savedValue\n        Just v -> v"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Maybe.Extra
a =
    maybeValue
        |> Maybe.Extra.withDefaultLazy (\\() -> Maybe.withDefault (Maybe.withDefault empty autoValue) savedValue)
"""
                        ]
        ]


shouldNotChangeTests : Test
shouldNotChangeTests =
    describe "should not change"
        [ test "case with non-Nothing default and transformation" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> defaultValue
        Just x -> Just (f x)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "case with additional patterns" <|
            \() ->
                """module A exposing (..)
a =
    case result of
        Err e -> Nothing
        Ok x -> Just x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "nested case in Just branch" <|
            \() ->
                """module A exposing (..)
a =
    case maybeValue of
        Nothing -> Nothing
        Just x ->
            case x of
                0 -> Nothing
                n -> Just n
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "case on non-Maybe type" <|
            \() ->
                """module A exposing (..)
a =
    case list of
        [] -> Nothing
        x :: xs -> Just x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


mapError : String -> Review.Test.ExpectedError
mapError under =
    Review.Test.error
        { message = "Use Maybe.map instead of case expression"
        , details = [ "This case expression can be simplified using Maybe.map with a pipe." ]
        , under = under
        }


andThenError : String -> Review.Test.ExpectedError
andThenError under =
    Review.Test.error
        { message = "Use Maybe.andThen instead of case expression"
        , details = [ "This case expression can be simplified using Maybe.andThen with a pipe." ]
        , under = under
        }


withDefaultError : String -> Review.Test.ExpectedError
withDefaultError under =
    Review.Test.error
        { message = "Use Maybe.withDefault instead of case expression"
        , details = [ "This case expression can be simplified using Maybe.withDefault." ]
        , under = under
        }


withDefaultLazyError : String -> Review.Test.ExpectedError
withDefaultLazyError under =
    Review.Test.error
        { message = "Use Maybe.Extra.withDefaultLazy instead of case expression"
        , details = [ "This case expression can be simplified using Maybe.Extra.withDefaultLazy." ]
        , under = under
        }


edgeCaseTests : Test
edgeCaseTests =
    describe "edge cases"
        [ describe "keeps the source of the branches as written"
            [ test "parentheses that set operator precedence" <|
                \() ->
                    """module A exposing (..)
a =
    case maybeValue of
        Nothing -> Nothing
        Just x -> Just ((x + 1) * 2)
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ mapError """case maybeValue of
        Nothing -> Nothing
        Just x -> Just ((x + 1) * 2)"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.map (\\x -> (x + 1) * 2)
"""
                            ]
            , test "field access on a parenthesized call" <|
                \() ->
                    """module A exposing (..)
a =
    case maybeValue of
        Nothing -> Nothing
        Just x -> Just (f x).name
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ mapError """case maybeValue of
        Nothing -> Nothing
        Just x -> Just (f x).name"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.map (\\x -> (f x).name)
"""
                            ]
            , test "escape sequences in a string literal" <|
                \() ->
                    """module A exposing (..)
a =
    case maybeValue of
        Nothing -> Nothing
        Just s -> Just (s ++ "\\n")
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ mapError """case maybeValue of
        Nothing -> Nothing
        Just s -> Just (s ++ "\\n")"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.map (\\s -> s ++ "\\n")
"""
                            ]
            , test "escaped quotes in a string default" <|
                \() ->
                    """module A exposing (..)
a =
    case maybeValue of
        Nothing -> "say \\"hi\\""
        Just s -> s
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ withDefaultError """case maybeValue of
        Nothing -> "say \\"hi\\""
        Just s -> s"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.withDefault "say \\"hi\\""
"""
                            ]
            , test "escape sequence in a char default" <|
                \() ->
                    """module A exposing (..)
a =
    case maybeValue of
        Nothing -> '\\n'
        Just c -> c
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ withDefaultError """case maybeValue of
        Nothing -> '\\n'
        Just c -> c"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.withDefault '\\n'
"""
                            ]
            , test "a case expression inside Just" <|
                \() ->
                    """module A exposing (..)
a =
    case maybeValue of
        Nothing -> Nothing
        Just x ->
            Just
                (case x of
                    0 -> "zero"
                    _ -> "other"
                )
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ mapError """case maybeValue of
        Nothing -> Nothing
        Just x ->
            Just
                (case x of
                    0 -> "zero"
                    _ -> "other"
                )"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.map (\\x -> case x of
                    0 -> "zero"
                    _ -> "other")
"""
                            ]
            , test "a comment inside the kept expression" <|
                \() ->
                    """module A exposing (..)
a =
    case maybeValue of
        Nothing -> Nothing
        Just x -> Just (f {- keep -} x)
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ mapError """case maybeValue of
        Nothing -> Nothing
        Just x -> Just (f {- keep -} x)"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeValue
        |> Maybe.map (\\x -> f {- keep -} x)
"""
                            ]
            , test "a comment the fix would drop: report without a fix" <|
                \() ->
                    """module A exposing (..)
a =
    case maybeValue of
        Nothing ->
            -- nothing to show
            Nothing

        Just x -> Just (f x)
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ mapError """case maybeValue of
        Nothing ->
            -- nothing to show
            Nothing

        Just x -> Just (f x)"""
                            ]
            ]
        , describe "parenthesizes where the pipeline would parse differently"
            [ test "case as the argument of <|" <|
                \() ->
                    """module A exposing (..)
a =
    String.toUpper <|
        case maybeValue of
            Nothing -> ""
            Just s -> s
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ withDefaultError """case maybeValue of
            Nothing -> ""
            Just s -> s"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    String.toUpper <|
        (maybeValue
            |> Maybe.withDefault "")
"""
                            ]
            , test "case as the right operand of ++" <|
                \() ->
                    """module A exposing (..)
a =
    "prefix: " ++ case maybeValue of
        Nothing -> ""
        Just s -> s
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ withDefaultError """case maybeValue of
        Nothing -> ""
        Just s -> s"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    "prefix: " ++ (maybeValue
                      |> Maybe.withDefault "")
"""
                            ]
            , test "subject using <|" <|
                \() ->
                    """module A exposing (..)
a =
    case lookup <| key of
        Nothing -> 0
        Just n -> n
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ withDefaultError """case lookup <| key of
        Nothing -> 0
        Just n -> n"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    (lookup <| key)
        |> Maybe.withDefault 0
"""
                            ]
            , test "subject that is an if expression" <|
                \() ->
                    """module A exposing (..)
a =
    case if flag then first else second of
        Nothing -> 0
        Just n -> n
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ withDefaultError """case if flag then first else second of
        Nothing -> 0
        Just n -> n"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    (if flag then first else second)
        |> Maybe.withDefault 0
"""
                            ]
            ]
        , describe "flattens only the pipe steps that keep the same meaning"
            [ test "a step that uses the bound variable stays in the lambda" <|
                \() ->
                    """module A exposing (..)
a =
    case maybeId of
        Nothing -> Nothing
        Just id -> lookup id |> Maybe.map (\\item -> ( id, item ))
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ andThenError """case maybeId of
        Nothing -> Nothing
        Just id -> lookup id |> Maybe.map (\\item -> ( id, item ))"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeId
        |> Maybe.andThen (\\id -> lookup id |> Maybe.map (\\item -> ( id, item )))
"""
                            ]
            , test "a step that can turn Nothing into Just stays in the lambda" <|
                \() ->
                    """module A exposing (..)
a =
    case maybeId of
        Nothing -> Nothing
        Just id -> lookup id |> Maybe.Extra.or fallback
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ andThenError """case maybeId of
        Nothing -> Nothing
        Just id -> lookup id |> Maybe.Extra.or fallback"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeId
        |> Maybe.andThen (\\id -> lookup id |> Maybe.Extra.or fallback)
"""
                            ]
            , test "only the trailing Maybe.map/andThen steps are flattened" <|
                \() ->
                    """module A exposing (..)
a =
    case maybeId of
        Nothing -> Nothing
        Just id -> lookup id |> withFallback |> Maybe.map .name
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ andThenError """case maybeId of
        Nothing -> Nothing
        Just id -> lookup id |> withFallback |> Maybe.map .name"""
                                |> Review.Test.whenFixed """module A exposing (..)
a =
    maybeId
        |> Maybe.andThen (\\id -> lookup id |> withFallback)
        |> Maybe.map .name
"""
                            ]
            ]
        , describe "Maybe.Extra import"
            [ test "adds the import after the last import when it is missing" <|
                \() ->
                    """module A exposing (..)
import Dict
a =
    case maybeValue of
        Nothing -> compute model
        Just x -> x
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ withDefaultLazyError """case maybeValue of
        Nothing -> compute model
        Just x -> x"""
                                |> Review.Test.whenFixed """module A exposing (..)
import Dict
import Maybe.Extra
a =
    maybeValue
        |> Maybe.Extra.withDefaultLazy (\\() -> compute model)
"""
                            ]
            , test "adds the import after the module documentation" <|
                \() ->
                    """module A exposing (a)

{-| Docs
-}


{-| A
-}
a =
    case maybeValue of
        Nothing -> compute model
        Just x -> x
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ withDefaultLazyError """case maybeValue of
        Nothing -> compute model
        Just x -> x"""
                                |> Review.Test.whenFixed """module A exposing (a)

{-| Docs
-}
import Maybe.Extra


{-| A
-}
a =
    maybeValue
        |> Maybe.Extra.withDefaultLazy (\\() -> compute model)
"""
                            ]
            , test "uses the alias Maybe.Extra is imported as" <|
                \() ->
                    """module A exposing (..)
import Maybe.Extra as MaybeX
a =
    case maybeValue of
        Nothing -> compute model
        Just x -> x
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ withDefaultLazyError """case maybeValue of
        Nothing -> compute model
        Just x -> x"""
                                |> Review.Test.whenFixed """module A exposing (..)
import Maybe.Extra as MaybeX
a =
    maybeValue
        |> MaybeX.withDefaultLazy (\\() -> compute model)
"""
                            ]
            ]
        , describe "does not report"
            [ test "a tail call to the enclosing function" <|
                \() ->
                    """module A exposing (..)
lastNode node =
    case node.next of
        Nothing -> Nothing
        Just next -> lastNode next
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            , test "a tail call to the enclosing let function" <|
                \() ->
                    """module A exposing (..)
a =
    let
        go m =
            case m of
                Nothing -> Nothing
                Just n -> go (step n)
    in
    go start
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            , test "constructors named Just and Nothing from another module" <|
                \() ->
                    """module A exposing (..)
a =
    case value of
        Remote.Nothing -> Remote.Nothing
        Remote.Just x -> Remote.Just x
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            ]
        , test "still reports a recursive call that is not a tail call" <|
            \() ->
                """module A exposing (..)
render node =
    case node.child of
        Nothing -> Nothing
        Just child -> Just (render child)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ mapError """case node.child of
        Nothing -> Nothing
        Just child -> Just (render child)"""
                            |> Review.Test.whenFixed """module A exposing (..)
render node =
    node.child
        |> Maybe.map (\\child -> render child)
"""
                        ]
        ]
