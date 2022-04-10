wai-middleware-comet
----

```haskell
import Wai.Middleware.Comet qualified

main = do
  comet <- Wai.Middleware.Comet.create
  run $ comet app
```

This middleware looks at the `X-Comet-Wait` header. If this is set to `true`, it waits for the resource (path) to be updated.
When the inner application returns 2xx response for any of POST, PUT, PATCH or DELETE request, the resource is considered updated.