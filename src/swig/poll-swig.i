/* -*- text -*- */
%module "poll-swig"

#define __BEGIN_DECLS
#define __END_DECLS
#define _SYS_POLL_H

%insert(lisphead) %{
(in-package :xcb)
%}

%feature("intern_function","custom-lispify");
%feature("export");

// We'll define these manually:
%ignore poll;
%ignore pollfd;

%include "bits/poll.h"

#undef _SYS_POLL_H
%include "sys/poll.h"
