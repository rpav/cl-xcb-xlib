# cl-xcb-xlib

This is a wrapper around [XCB](http://xcb.freedesktop.org/) and
[Xlib/XCB](http://xcb.freedesktop.org/XlibXcb/) that implements **a
drop-in replacement for [CLX](http://www.cliki.net/CLX)**.

## Why?

The rationale behind this is simple:

* There are many nice libraries that depend on `libX11` structures
  such as `Display` and `Screen` pointers.

* `libGL` is one of these, especially if you want *direct rendering*.

* CLX is an existing standard.

Thus, it would be nice if we could write to CLX, but somehow obtain
`libX11`-compatible data structures.  Meet clx-xcb-xlib.

## How?

XCB, similar to CLX, is a reimplementation of the X protocol, but for
C.  It, too, is not directly compatible with `libX11` and cannot
directly interface with things like OpenGL (excepting pure indirect
GLX), but the compatibility layer, Xlib/XCB, *can*.  Many new things
are including XCB support directly.

The choice of wrapping XCB rather than directly wrapping `libX11` was
mostly due to its far superior error handling; it will not force-exit
a process when it encounters an I/O error.  This makes it much more
Lisp-friendly.

(Unfortunately, things which call into `libX11` can still result in
fatal errors.  Error handling is in place which prevents an immediate
exit.  This may result in an undefined `libX11` state, but will at
least allow some analysis and cleanup before restarting Lisp.)

## What?

CLX support is not 100%, but tries to be as close as possible.
Work is still underway to complete the API, and it is getting very
close.  Generally not implemented, but coming soon:

    - Images (chapter 7)
    - Control (chapter 14)
    - Extensions beyond GLX (though list-extensions and
      query-extensions are implemented)

Unlikely to be implemented:

    - Resources (chapter 13)

Unimplemented functions are listed in `xlib:*unimplemented*`.  There
are probably a few stragglers not listed above, but the rest of the
API should be in place.

This may not be enough to run all existing CLX applications, and there
are (a very few) known-broken return formats.  However this should be
more than enough for new apps.

Some things are not directly compatible; for instance, `declare-event`
requires an XCB struct name, and has slightly different type syntax in
some cases.  Incompatibilities should be noted in
`doc/incompatibilities.txt`.  Perhaps of greatest note is GL/GLX
support, which requires `cl-opengl`, and due to newer OpenGL APIs,
probably requires reworking GLX calls when coming from `portable-clx`.
