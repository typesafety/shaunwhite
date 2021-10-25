# shaunwhite

This bot uses simmsb's [Calamity](https://github.com/simmsb/calamity) library.

## TODO

* Write a custom (more verbose) `help` command
* Let users remove roles assigned to them
* Schedule messages to new @reminders role
* Hoogle command
* File structure at some point
* <sub>Write tests</sub>

## Custom prelude

The `src/CustomPrelude.hs` module implements the custom prelude used in the
project and consists of minor additions/changes to Kowainik's
[Relude](https://github.com/kowainik/relude) package.

The `-XNoImplicitPrelude` extension is enabled by default to avoid importing
the default Haskell prelude. Explicitly import the custom prelude for each
module instead:

```haskell
module Example
    ( ...
    ) where

import CustomPrelude
```

Relude provides the `Relude.Extra` module which contains some extra stuff that
can be imported in addition to `CustomPrelude`, if so desired.

## Language extensions

Check `package.yaml` for language extensions enabled by default.

## Structure

The `src` directory holds the source code, with `Shaunwhite.hs` being the entry
point to the application.

# Building

`shaunwhite` is built with [Stack](https://www.haskellstack.org).

Build the bot:

```sh
stack build
```

Run the bot:

```sh
stack run -- --token path/to/bot/token
```
