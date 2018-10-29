# yampa-gloss

[![Build Status](https://travis-ci.org/ivanperez-keera/yampa-gloss.svg?branch=master)](https://travis-ci.org/ivanperez-keera/yampa-gloss)
[![Version on Hackage](https://img.shields.io/hackage/v/yampa-gloss.svg)](https://hackage.haskell.org/package/yampa-gloss)
[![Flattr this](http://api.flattr.com/button/flattr-badge-large.png "Flattr This!")](https://flattr.com/submit/auto?user_id=ivanperez-keera&url=https://github.com/ivanperez-keera/yampa-gloss&title=yampa-gloss&language=&tags=github&category=software)

A Gloss backend for Yampa.

Gloss is a purely functional library to create pictures and animations.  Yampa
is a Functional Reactive Programming DSL structured using arrow combinators.

This library provides a function to create an interactive gloss animation
driven by a signal function that transforms a Gloss input signal into a Gloss
Picture.

## Installation

yampa-gloss is available on hackage: http://hackage.haskell.org/package/yampa-gloss.

```
$ cabal sandbox init         # Optional, but recommended
$ cabal update
$ cabal install yampa-gloss
```

## Examples

There is a directory with one example. You can install it with:

```
$ cabal sandbox init         # Optional, but recommended
$ cabal update
$ cabal install yampa-gloss -fexamples
$ ./.cabal-sandbox/bin/yampa-examples-gloss-rotatingcolor
```

* See also:
  * [Yampa](https://github.com/ivanperez-keera/Yampa)
  * [Yampa2048](https://github.com/ksaveljev/yampa-2048)

## Help and collaboration

You can collaborate at least in three ways:

* File an issue (https://github.com/ivanperez-keera/Yampa/issues).
* Write documentation (send a link and/or a pull request).

## Authors

* Konstantin Saveljev (library)
* Ivan Perez (example)

### Maintainer

* Ivan Perez

### Acknowledgements

This work is a fork off [Yampa2048](https://github.com/ksaveljev/yampa-2048),
created by Konstantin Saveljev.  Yampa2048 was inspired by work done by Josh
Kirklin and Maia Werbos. See the Yampa2048 repo for details.
