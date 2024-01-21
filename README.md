# xmonad-decoration-cairo

This repository contains modules to draw nice window decorations for XMonad by use of cairo library.

![Screenshot_20231219_011601](https://github.com/portnov/xmonad-decoration-ex/assets/284644/e6fb74d9-8894-4002-a7ad-f399a218bb0f)

Here a new `DecorationEngine` is implemented, which draws window decorations
via cairo. It can paint decoration borders, horizontal or vertical gradients,
and PNG images (tiled or scaled). Decoration engine can be combined with
different `DecorationGeometry` instances, to get, for example, either usual
bars above windows or tabs, painted with the same theme.

## Current state

This package is currently under active development.

* xmonad-decoration-cairo will probably be included into xmonad-extras, as it
  requires `cairo` package, which takes some time to build (and, probably, is
  not available on all platforms). Until then, xmonad-decoration-cairo exist as
  a separate package.

It is probable that inclusion of this package into xmonad-extra will require
some refactoring. So please be warned, that if you use these packages in their
current state, you will probably have to update your config once these packages
will be merged.

## Usage

Add to your `stack.yaml` in your xmonad config directory:

```
extra-deps:
  - git: https://github.com/portnov/xmonad-decoration-cairo.git
    commit: 12..... (you have to put full SHA1 of last commit here)
```

Alternatively, you can clone these repositories to your machine and write

```
extra-deps:
    - /home/user/src/xmonad-decoration-cairo
```

After that, in your `xmonad.hs`, write something like

```
import XMonad.Layout.DecorationEx.Cairo

myTheme = CairoTheme {...}

myL = cairoDecoration shrinkText myTheme (layoutHook def)

main = xmonad def { layoutHook = myL }
```

## License

BSD-3, see LICENSE.

