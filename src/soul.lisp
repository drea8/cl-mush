(in-package :mush)

(defthing soul (ghost)
  conn
  know
  soulhight
  password
  id
  soul-new
  embodied
  )

(defun soul-data (soul)
  `((soulhight ,(soulhight soul))
    (password ,(password soul))
    (hight ,(hight soul))
    (sight ,(sight soul))
    (poolid ,(poolid soul))
    (keys (list ,(string-downcase (hight soul))))    
   ))


(defun random-pool ()
  (id (nth (random (length pools)) pools)))


(defun new-soul (conn)
  (print `(soul of ,conn))
  (make-instance
   'soul
   
   :soulhight '()
   :password '()
   :soul-new nil
   :embodied nil

   :hight nil
   :rsight "a sickly pale ghost floats here and there."		 
   :sight "A faint greenish apparition floating about."
   :poolid (random-pool)   
   
   :know 1   
   :conn conn
   
   :ich (uuid-integer)
   :keys (list "ghost")))
  

(defun write-souls ()
  (print `(writing soul data))
  (with-open-file (stream (c+ root-dir "souls/souls.sexp")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "(~%")
    (loop for soul in souls-accounts do
	 (write (soul-data soul) :stream stream :escape t)
	 (format stream "~%"))
    (format stream ")")))

(defun souls-pool (pool)
  (remove-if-not (lambda (x) (typep x 'soul))
		 (things pool)))


(defparameter souls-accounts '())


(defun load-souls ()
  (setq souls-accounts (read-things-file 'soul "souls/souls.sexp")))

(load-souls)
