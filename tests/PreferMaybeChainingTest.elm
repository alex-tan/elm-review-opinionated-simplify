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
        |> Maybe.map ((\\item ->
            let
                detail = item.name
            in
            detail
        ))
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
