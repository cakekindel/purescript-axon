# axon

**WIP**

HTTP server library inspired by [`axum`](https://docs.rs/latest/axum), allowing best-in-class
expressive routing.

The main difference between this server library compared to others (eg. the wonderful [`httpurple`](https://github.com/sigma-andex/purescript-httpurple))
is the philosophy around **routing**.

The core abstraction is a [`Handler`](./src/Axon.Request.Handler.purs); any function with the type `[...Request parts] -> m Response`.

This allows each REST action to correspond to a single function, which declares its requirements in its type signature.
This allows for a highly refactorable & composable application, as opposed to a hierarchical routing approach like `routing-duplex`.

For example, an endpoint `GET /persons/:id/address` would be modeled as:

```purs
getPersonAddress :: Get -> Path ("persons" / Int / "address") Int -> Aff Response
getPersonAddress _ (Path id) = ...
```

`POST /persons` accepting a json body:

```purs
type Person = { firstName :: String, lastName :: String, age :: Maybe Int }

postPerson :: Post -> Path "persons" Unit -> ContentType Json -> Json Person -> Aff Response
postPerson _ _ person = ...
```

Then these can be rolled up into a `/persons` resource with `Handler.or`:
```purs
persons :: Handler Aff Response
persons = getPerson `Handler.or` postPerson `Handler.or` deletePerson `Handler.or` getPersonAddress ...
```

Then run with:
```purs
Axon.serveNode {port: 10000, hostname: "0.0.0.0"} persons
```

## Example

This example implements this REST interface in 36LoC:

- `GET /cheeses` - Lists all cheeses (strings) known to server
- `POST /cheeses` - Add a cheese to the cheese list
- `DELETE /cheeses/:cheese` - Remove a cheese from the cheese list

```purs
module Main where

import Prelude

import Axon as Axon
import Axon.Request.Handler as Handler
import Axon.Request.Parts.Class (Delete, Get, Path(..), Post)
import Axon.Request.Parts.Path (type (/))
import Axon.Response (Response)
import Axon.Response.Construct (Json(..), toResponse)
import Axon.Response.Status as Status
import Data.Filterable (filter)
import Data.Foldable (elem)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, joinFiber)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

main :: Effect Unit
main = launchAff_ do
  cheeses :: Ref (Array String) <- liftEffect $ Ref.new
    [ "cheddar", "swiss", "gouda" ]

  let
    getCheeses :: Get -> Path "cheeses" _ -> Aff Response
    getCheeses _ _ = liftEffect do
      cheeses' <- Ref.read cheeses
      toResponse $ Status.ok /\ Json cheeses'

    deleteCheese :: Delete -> Path ("cheeses" / String) _ -> Aff Response
    deleteCheese _ (Path id) = liftEffect do
      cheeses' <- Ref.read cheeses
      if not $ elem id cheeses' then
        toResponse Status.notFound
      else
        Ref.modify_ (filter (_ /= id)) cheeses
        *> toResponse Status.accepted

    postCheese :: Post -> Path "cheeses" _ -> String -> Aff Response
    postCheese _ _ cheese =
      let
        tryInsert as
          | elem cheese as = { state: as, value: false }
          | otherwise = { state: as <> [ cheese ], value: true }
      in
        liftEffect
          $ Ref.modify' tryInsert cheeses
          >>= if _ then toResponse Status.accepted else toResponse Status.conflict

  handle <-
    Axon.serveBun
      { port: 8080, hostname: "localhost" }
      (getCheeses `Handler.or` postCheese `Handler.or` deleteCheese)

  joinFiber handle.join
```

## Request Handlers

Request handler functions have any number of parameters that are `RequestParts` and return an `Aff Response` (or any `MonadAff`).

<details>
<summary>

`RequestParts`

</summary>

- `Request`
  - Always succeeds; provides the entire request
- **Combinators**
  - `Unit`
    - Always succeeds
  - `a /\ b`
    - Tuple of `a` and `b`, where `a` and `b` are `RequestParts`.
  - `Maybe a`
    - `a` must be `RequestParts`. If `a` can't be extracted, the handler will still succeed and this will be `Nothing`. If `a` was extracted, it's wrapped in `Just`.
  - `Either a b`
    - `a` and `b` must be `RequestParts`. Succeeds if either `a` or `b` succeeds (preferring `a`). Fails if both fail.
- **Body**
  - `String`
    - succeeds when request has a non-empty body that is valid UTF-8
  - `Json a`
    - succeeds when request has a `String` body (see above) that can be parsed into `a` using `DecodeJson`.
  - `Buffer`
    - succeeds when request has a nonempty body.
  - `Stream`
    - succeeds when request has a nonempty body.
- **Headers**
  - `Header a`
    - `a` must be `TypedHeader` from `Axon.Header.Typed`. Allows statically (ex. `ContentType Type.MIME.Json`) or dynamically (ex. `ContentType String`) matching request headers.
  - `HeaderMap`
    - All headers provided in the request
- **Path**
  - `Path a c`
    - Statically match the path of the request, and extract parameters. See `Axon.Request.Parts.Path`. (TODO: this feels too magical, maybe follow axum's prior art of baking paths into the router declaration?)
- **Method** - `Get` - `Post` - `Put` - `Patch` - `Delete` - `Options` - `Connect` - `Trace`
</details>

Similarly to the structural extraction of request parts; handlers can use `Axon.Response.Construct.ToResponse` for easily constructing responses.

<details>
<summary>

`ToResponse`

</summary>

- **Combinators**
  - `Status /\ a`
    - Special case to make sure any `Status` in a tuple will take priority over any default statuses within. TODO: This case (overlapping with `a /\ b` requires the class to be "sealed" in an instance chain. Want a clean way around this so consumers can implement `ToResponse`.)
  - `a /\ b`
    - Merges `toResponse a` and `toResponse b`, using `b` on conflicts
- **Status**
  - `Axon.Response.Status.Status`
- **Body**
  - `Axon.Response.Body.Body`
  - `String`
  - `Node.Buffer.Buffer`
  - `Node.Stream.Readable a` (for all `a`)
  - `Axon.Response.Construct.Json a`
    - `a` must be `EncodeJson`. This will set the body to `a` stringified, and set `Content-Type` to `application/json`.
- **Headers**
  - `ToResponse` is implemented for all implementors of `TypedHeader`
  - TODO: `Map String String`
  </details>
