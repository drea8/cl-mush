(in-package :mush)

(defthing soul (ghost)
  conn
  know
  soulhight
  password  
  )
  

(defun new-soul (conn)
  (print `(soul of ,conn))
  (let* ((hight "Kjenn"))
  (make-instance
   'soul
   :soulhight "user"
   :password "pass"
   :hight hight
   :know 1
   :poolid 4
   :conn conn
   :ich (uuid-integer)
   :rsight "a sickly pale ghost floats here and there."		 
   :sight "A faint greenish apparition floating about."
   :keys (list (string-downcase hight)))))
  


(defun souls-pool (pool)
  (remove-if-not (lambda (x) (typep x 'soul))
		 (things pool)))
   

