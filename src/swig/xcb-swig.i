/* -*- text -*- */
%module "xcb-swig"

#define _XFUNCPROTOBEGIN
#define _XFUNCPROTOEND
#define __extension__

%insert(lisphead) %{
(in-package :xcb)
%}

%feature("intern_function","custom-lispify");
%feature("export");

%include "stdint.h"
%include "xcb/xcb.h"
%include "xcb/xproto.h"
%include "X11/Xlib-xcb.h"

