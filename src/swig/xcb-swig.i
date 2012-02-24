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

/* This does not return NULL-terminated strings and therefore builtin
   translation breaks */
void* xcb_get_atom_name_name(void *R);
void* xcb_setup_request_authorization_protocol_data(void *R);
void* xcb_setup_request_authorization_protocol_name(void *R);
void* xcb_str_name(void *R);
void* xcb_list_fonts_with_info_name(void *R);

%ignore xcb_get_atom_name_name;
%ignore xcb_setup_request_authorization_protocol_data;
%ignore xcb_setup_request_authorization_protocol_name;
%ignore xcb_str_name;
%ignore xcb_list_fonts_with_info_name;

%include "stdint.h"
%include "xcb/xcb.h"
%include "xcb/xproto.h"
%include "X11/Xlib-xcb.h"
