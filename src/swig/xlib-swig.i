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

#define None 0L;
typedef unsigned long XID;
typedef unsigned long Window;

%include "X11/Xlib.h"
%include "X11/Xutil.h"
%include "GL/glx.h"

#define GLX_GLXEXT_PROTOTYPES
%include "GL/glxext.h"
