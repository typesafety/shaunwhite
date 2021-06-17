Check `package.yaml` for language extensions enabled by default. Notably,
`-XNoImplicitPrelude` is enabled to use
[Relude](https://github.com/kowainik/relude) instead.

This means that that the `Relude` module(s) must be explicitly imported.
As an example, the imports at the top of a module could look like this:

```haskell
module Example
    ( ...
    ) where

import Relude        -- For most imports normally provided by Prelude.
import Relude.Extra  -- Extra stuff provided by Relude
import Data.Bits     -- Package from `base` not exported by Relude.
```

# Building

`shaunwhite` is built with [Stack](https://www.haskellstack.org).

```sh
stack build
```
