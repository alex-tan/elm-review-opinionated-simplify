module SimplifyLambdaToCompositionTest exposing (all)

import Review.Test
import SimplifyLambdaToComposition exposing (rule)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "SimplifyLambdaToComposition"
        [ shouldSimplifyTests
        , shouldNotSimplifyTests
        ]


shouldSimplifyTests : Test
shouldSimplifyTests =
    describe "should simplify"
        [ test "simple function with field access" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\item -> String.length item.name) items
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Lambda can be simplified to function composition"
                            , details = [ "Instead of `(\\item -> String.length item.name)`, you can write `(String.length << .name)`." ]
                            , under = "\\item -> String.length item.name"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sortBy (String.length << .name) items
"""
                        ]
        , test "qualified module function" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\todo -> Time.posixToMillis todo.createdAt) todos
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Lambda can be simplified to function composition"
                            , details = [ "Instead of `(\\todo -> Time.posixToMillis todo.createdAt)`, you can write `(Time.posixToMillis << .createdAt)`." ]
                            , under = "\\todo -> Time.posixToMillis todo.createdAt"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sortBy (Time.posixToMillis << .createdAt) todos
"""
                        ]
        , test "deeply qualified module function" <|
            \() ->
                """module A exposing (..)
a = List.map (\\x -> Some.Deep.Module.func x.field) list
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Lambda can be simplified to function composition"
                            , details = [ "Instead of `(\\x -> Some.Deep.Module.func x.field)`, you can write `(Some.Deep.Module.func << .field)`." ]
                            , under = "\\x -> Some.Deep.Module.func x.field"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (Some.Deep.Module.func << .field) list
"""
                        ]
        , test "nested function application" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\item -> func1 (func2 item.name)) items
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Lambda can be simplified to function composition"
                            , details = [ "Instead of `(\\item -> func1 (func2 item.name))`, you can write `(func1 << func2 << .name)`." ]
                            , under = "\\item -> func1 (func2 item.name)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sortBy (func1 << func2 << .name) items
"""
                        ]
        , test "triple nested function application" <|
            \() ->
                """module A exposing (..)
a = List.map (\\x -> f (g (h x.field))) list
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Lambda can be simplified to function composition"
                            , details = [ "Instead of `(\\x -> f (g (h x.field)))`, you can write `(f << g << h << .field)`." ]
                            , under = "\\x -> f (g (h x.field))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (f << g << h << .field) list
"""
                        ]
        ]


shouldNotSimplifyTests : Test
shouldNotSimplifyTests =
    describe "should not simplify"
        [ test "lambda with multiple parameters" <|
            \() ->
                """module A exposing (..)
a = List.map (\\x y -> func x.field) items
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "lambda with different variable in field access" <|
            \() ->
                """module A exposing (..)
a = List.map (\\x -> func y.field) items
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "lambda with arithmetic in argument" <|
            \() ->
                """module A exposing (..)
a = List.map (\\x -> func (x.field + 1)) items
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "lambda with case expression" <|
            \() ->
                """module A exposing (..)
a = List.map (\\x -> case x.field of _ -> 1) items
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "lambda returning just the field" <|
            \() ->
                """module A exposing (..)
a = List.map (\\x -> x.field) items
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "lambda with tuple pattern" <|
            \() ->
                """module A exposing (..)
a = List.map (\\(x, y) -> func x.field) items
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
