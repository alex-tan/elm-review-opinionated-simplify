# elm-review-opinionated-simplify

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules for opinionated simplification of Elm code.

## Provided rules

- [`PreferMaybeChaining`](https://package.elm-lang.org/packages/alex-tan/elm-review-opinionated-simplify/1.0.0/PreferMaybeChaining) - Reports case expressions on Maybe that can be simplified using `Maybe.map`, `Maybe.andThen`, `Maybe.withDefault`, or `Maybe.Extra.withDefaultLazy`.
- [`SimplifyLambdaToComposition`](https://package.elm-lang.org/packages/alex-tan/elm-review-opinionated-simplify/1.0.0/SimplifyLambdaToComposition) - Reports lambdas like `(\x -> func x.field)` that can be simplified to function composition `(func << .field)`.

## Configuration

```elm
module ReviewConfig exposing (config)

import PreferMaybeChaining
import Review.Rule exposing (Rule)
import SimplifyLambdaToComposition

config : List Rule
config =
    [ PreferMaybeChaining.rule
    , SimplifyLambdaToComposition.rule
    ]
```

## Examples

### PreferMaybeChaining

#### Maybe.map

```elm
-- Before
case maybeValue of
    Nothing -> Nothing
    Just x -> Just (f x)

-- After
maybeValue
    |> Maybe.map (\x -> f x)
```

#### Maybe.andThen

```elm
-- Before
case maybeId of
    Nothing -> Nothing
    Just id -> Store.findById id store

-- After
maybeId
    |> Maybe.andThen (\id -> Store.findById id store)
```

Pipe chains are flattened:

```elm
-- Before
case maybeId of
    Nothing -> Nothing
    Just id -> lookup id |> Maybe.map .name

-- After
maybeId
    |> Maybe.andThen (\id -> lookup id)
    |> Maybe.map .name
```

#### Maybe.withDefault

```elm
-- Before
case maybeValue of
    Nothing -> 0
    Just x -> x

-- After
maybeValue
    |> Maybe.withDefault 0
```

#### Maybe.Extra.withDefaultLazy

Used when the default expression is not a simple literal or variable:

```elm
-- Before
case maybeValue of
    Nothing -> computeDefault model
    Just x -> x

-- After
maybeValue
    |> Maybe.Extra.withDefaultLazy (\() -> computeDefault model)
```

### SimplifyLambdaToComposition

```elm
-- Before
List.sortBy (\todo -> Time.posixToMillis todo.createdAt) todos

-- After
List.sortBy (Time.posixToMillis << .createdAt) todos
```

Nested function applications are also handled:

```elm
-- Before
List.map (\x -> f (g x.field)) list

-- After
List.map (f << g << .field) list
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template alex-tan/elm-review-opinionated-simplify/example
```
