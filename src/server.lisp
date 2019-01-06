(defun mush-dependencies ()
  (ql:quickload '(:usocket
		  :cl-ppcre
		  :bordeaux-threads
		  :local-time)))

(defpackage :mush
  (:use :cl :usocket :bordeaux-threads))

(in-package :mush)

(load "config.sexp")
(load "src/utils.lisp")
(load "src/pools.lisp")
(load "src/thing.lisp")
(load "src/ghost.lisp")
(load "src/soul.lisp")
(load "src/client.lisp")

(defparameter server-thread nil)
(defparameter server-socket nil)
(defparameter server-running nil)
(defparameter connections nil)
(defparameter users nil)

(defthing mush-user (stream-usocket)
  usocket-stream
  soul
  ip
  last-packets-time
  packet-counter
  last-doing)

(defun make-new-connection (usocket-stream conn-stream log-stream)
  (print `(connected ,(get-peer-address usocket-stream)
		     ,(get-peer-port usocket-stream)))
  (push `(,usocket-stream
	  ,(make-instance 'mush-user
			 :usocket-stream usocket-stream
			 :soul nil
			 :packet-counter 0
			 :last-packets-time (get-universal-time)
			 :ip (get-peer-address usocket-stream) ))
	users)
  (welcome-connection conn-stream)
  usocket-stream)


(defun close-connection (conn)
  (socket-close conn)
  (setf connections (remove conn connections)))


(defun conn-user (usocket-stream)
  (second (assoc usocket-stream users :test #'equal)))

(defun timestamp ()
  (local-time:universal-to-timestamp (get-universal-time)))

(defun overflow-check (user)
  (let ((now (get-universal-time)))
    (setf (packet-counter user) (1+ (packet-counter user)))
    (cond ((> (packet-counter user) 10)
	   (setf (packet-counter user) 0)	 
	   (cond ((< (- now (last-packets-time user)) 3)
		(print `(,user packet rate exceeded))
		  (send (soul user) "Packet rate exceeded, you might be a bot or spam, bye.")
		  (clean-soul (soul user))
		  (error 'SB-INT:SIMPLE-STREAM-ERROR)))
	   (setf (last-packets-time user) now)))))


(defun existing-connection (socket stream user)
  (overflow-check user)
  (handle-user user socket stream))


(defun server-loop (which-socket port &optional (log-stream *standard-output*))
  (eval `(setq ,which-socket (socket-listen "0.0.0.0" ,port :reuse-address t)))
  (setq connections (list (symbol-value which-socket)))
  (let* ((server-running t))    
    (loop until (not server-running) do
	 (loop for conn in (wait-for-input connections :ready-only t)
	    until (not server-running) do
	      (unwind-protect
		   (handler-case 
		       (progn
			 (if (typep conn 'stream-server-usocket)			     
			     (let* ((usocket-stream (socket-accept conn))
				    (conn-stream (socket-stream usocket-stream)))
			       (setq new-conn (make-new-connection usocket-stream conn-stream log-stream))
			       (push new-conn connections)
			       (setq conn new-conn)))
			 			 
			 (let* ((socket conn)			    				     
				(stream (socket-stream conn))
				(user (conn-user conn)))
			   (existing-connection socket stream user)))
		     
		     (SB-BSD-SOCKETS:NOT-CONNECTED-ERROR ()
		       (print "not-connected")
		       (setq connections (remove conn connections :test #'equal))
		       (socket-close conn))
		     (SB-INT:CLOSED-STREAM-ERROR ()
		       (print "closed-stream")
		       (setq connections (remove conn connections :test #'equal))
		       (socket-close conn))
		     (SB-INT:SIMPLE-STREAM-ERROR ()
		       (print "broken-pipe-error")
		       (setq connections (remove conn connections :test #'equal))
		       (socket-close conn))		       		      		
		     ))))))
    

(defun mush-start ()
  (setq server-thread
	(bt:make-thread
	 (lambda () (server-loop 'server-socket 4444)) :name "mush-server")))
  

(defun mush-stop ()
  (bt:destroy-thread server-thread)
  (ignore-errors (socket-close server-socket))
  (setq server-running nil
	connections '() ) 
  (print '(SERVER THREAD DESTROYED)))

(defun mush-freeze ()
  (bt:destroy-thread server-thread))

(defun mush-restart ()
  (mush-stop)
  (mush-start))

(mush-start)

