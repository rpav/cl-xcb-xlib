# cl-xcb-xlib

This is a wrapper around [XCB](http://xcb.freedesktop.org/) and
[Xlib/XCB](http://xcb.freedesktop.org/XlibXcb/) that implements **a
drop-in replacement for [CLX](http://www.cliki.net/CLX)**.

## Usage

### Requirements

* XCB, of course; if you're using a recent Xorg, you probably have
  this already

* Lisp: cffi/libffi branch, ChanL, static-vectors, trivial-garbage

* Not strictly necessary: cl-opengl if you want to use GLX, cl-cairo2
  if you want cairo-xcb integration.

**Build this in a very fast console, or turn down SBCL verbosity.** It
normally builds reasonably fast, but if you try and run this in SLIME
or a slow terminal with the compiler noting every top-level form, it
will take forever.  This imports the majority of Xlib and XCB, and the
wrappers are nearly 21k lines and growing.

You may notice style-warnings for some undefined aliens; I get a list
of some SGIX/MESA GL extensions.  Nothing critically necessary should
be on this list.

This has currently only been tested on SBCL, but more to come.

### Demos, or "I want to see it work!"

I've started writing [some
demos](https://github.com/rpav/cl-xcb-xlib-demos), which should
illustrate some basic things.

## About

### Why?

The rationale behind this is simple:

* There are many nice libraries that depend on `libX11` structures
  such as `Display` and `Screen` pointers.

* `libGL` is one of these, especially if you want *direct rendering*.

* CLX is an existing standard.

Thus, it would be nice if we could write to CLX, but somehow obtain
`libX11`-compatible data structures.  Meet clx-xcb-xlib.

### How?

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
fatal errors, but error handling is in place to prevent an immediate
exit.)

### What?

CLX support is not 100%, but tries to be as close as possible.
Work is still underway to complete the API, and it is getting very
close.  Generally not implemented, but coming soon:

* ICCCM / NET_WM
* Extensions beyond GLX (though list-extensions and query-extensions
  are implemented)

Unlikely to be implemented:

* Resources (chapter 13)

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
