# CL-MUSH

* Common Lisp MUSH Environment Engine
* Multi-User TCP Server

# Install
* install quicklisp
* (ql:quickload '(:usocket :cl-ppcre :bordeaux-threads))
* (save-lisp-and-die "cl-mush.img")
* sbcl --core cl-mush.img --load "src/server.lisp"


# To Do
* design, refactor consideration, clean, consolidate
* soul-cmd refactor HANDLE-USER (soul user) etc
* interaction, pmake/mobmake shell
* auto saving (room, inv, stats)
* backups (pools/souls), atomic (no-delete) record