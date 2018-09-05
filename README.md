The examples from the original try.elm-lang.org site updated for Elm 0.19.

This is a cut-down version of the original site which provides the compiler API and code editor without any of the additional elm-lang.org content. A lot of the code is taken from https://github.com/elm/elm-lang.org with minor changes and as such is copyright the original authors.

## Building and running

There are makefiles for building the Elm code and HTML, so running `make` in the project root should create a directory called `static` which contains all the static website content.

I use nix for Haskell development, so my workflow is

```
nix-shell
cabal new-build
cabal new-run
```

which should run the site on port 8000. Check by loading one of the examples, e.g. http://localhost:8000/examples/buttons.
