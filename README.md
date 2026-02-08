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

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template alex-tan/elm-review-opinionated-simplify/example
```
