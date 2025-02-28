# axon

**WIP**

HTTP server library inspired by [`axum`](https://docs.rs/latest/axum), allowing best-in-class
expressive routing.

```purs
root :: Get -> Path "/" _ -> Aff String
root _ _ = pure "Hello, world!"

main :: Effect Unit
main = Axon.serve (root `Handle.or` Handle.Default.notFound)

-- GET localhost:8000/      -> 200 OK "Hello, world!"
-- GET localhost:8000/foo   -> 404 Not Found
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
