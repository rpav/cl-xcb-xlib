.PHONY: all install clean

TARGETS = xcb-swig.lisp xlib-swig.lisp

SWIG = /home/rpav/repository/svn/swig/trunk/preinst-swig

all: $(TARGETS)

install: all
	cp $(TARGETS) ..

clean:
	rm -f $(TARGETS)

%.lisp: %.i
	$(SWIG) -cffi -generate-typedef -I/usr/include $<

