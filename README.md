# Stitch [![Build Status](https://travis-ci.org/intolerable/stitch.svg?branch=master)](https://travis-ci.org/intolerable/stitch)

a tiny css dsl for haskell

this haskell code:
```haskell
import Stitch

style :: CSS
style = "body" ? do
  "h1" ? do
    "color" .= "#444"
    "a" ? do
      "color" .= "#448"
      "&:hover" ?
        "color" .= "#44F"
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
body h1 a {
  color: #448
}
body h1 a:hover {
  color: #44F
}
```

stitch doesn't do any type-checking of css properties and probably won't in the future. it's intended to be a flexible and more composable way of building stylesheets in haskell, and it also includes a monad transformer for potentially interesting shenanigans.

the core of the library is contained in the `StitchT` monad transformer, and the `renderCSS`, `?` and `.=` functions. the simplest way to use the library is simply to build up a CSS representation using the monad instance and then convert it to text with `renderCSS`. it's also possible to get a abstract representation of the CSS tree with `runStitchT`, but this is rarely useful.

stitch was designed to be a lighter version of [clay](https://github.com/sebastiaanvisser/clay) that's a bit more flexible and less limiting. it won't catch css errors at compile time, but it also won't add Prelude conflicts or prevent you from using some of the more obscure css features.

for a commented example of how to use the library, check `Stitch.Example`.
