#!/bin/sh
grep 'xcb_[a-z0-9_]*_cookie_t' xcb-swig.lisp | sed 's/.*xcb_\([a-z0-9_]*\)_cookie_t.*/\1/;s/_/-/g' | sort | uniq
