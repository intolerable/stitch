# Stitch

a tiny css dsl for haskell

this haskell code:
```haskell
import Stitch

style :: CSS
style = "body" ? do
  "h1" ? do
    "color" .= "#444"
    "a:hover" ?
      "color" .= "#448"
    "font" -: do
      "family" .= "Open Sans, sans"
      "size" .= "1.4em"
      "weight" .= "bold"
```

turns into this:
```
body h1 {
  color: #444;
  font-family: Open Sans, sans;
  font-size: 1.4em;
  font-weight: bold
}
body h1 a:hover {
  color: #448
}
```

stitch doesn't do any type-checking of css properties and probably won't in the future. it's intended to be a flexible and more composable way of building stylesheets in haskell, and it also includes a monad transformer for potentially interesting shenanigans

the core of the library is contained in the `StitchT` monad transformer, and the `renderCSS`, `?` and `.=` functions. the simplest way to use the library is simply to build up a CSS representation using the monad instance and then convert it to text with `renderCSS`. it's also possible to get a abstract representation of the CSS tree with `runStitchT`, but this is rarely useful.

for a commented example of how to use the library, check `Stitch.Example`.
