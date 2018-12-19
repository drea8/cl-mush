# CL-MUSH
* Common Lisp MUSH Environment Engine
* Multi-User TCP Server

# Install
* install quicklisp
* (ql:quickload '(:usocket :cl-ppcre :bordeaux-threads))
* (save-lisp-and-die "cl-mush.img")
* sbcl --core cl-mush.img --load "src/server.lisp"