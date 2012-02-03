/* -*- text -*- */
%module "xlib-swig"

#define _XFUNCPROTOBEGIN
#define _XFUNCPROTOEND
#define _Xconst const
#define _X_SENTINEL(x)

%insert(lisphead) %{
(cl:in-package :xcb)
%}

%feature("intern_function","custom-lispify");
%feature("export");

%include "X11/Xlib.h"
