
(in-package :mush)

(defparameter pools '() )


(defthing pool ()
   id
   hight
   sight
   paths
   things)



(defun pool-data (pool)
  `((id ,(id pool))
    (hight ,(hight pool))
    (sight ,(sight pool))
    (paths ,(paths pool))))



(defun write-pools ()
  (print `(writing pool data))
  (with-open-file (stream (c+ root-dir "pools.sexp")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "(~%")
    (loop for pool in pools do
	 (write (pool-data pool) :stream stream :escape t)
	 (format stream "~%"))
    (format stream ")")))


(defun pool (id)
  (first (remove-if-not
	  (lambda (pool) (= (id pool) id))
	  pools)))

(defun path-pool (room path-str)
  (second (assoc path-str (paths room) :test #'equal)))

(defun path-strs (pool)
  (mapcar #'first (paths pool)))


(defun path-match (msg)
  (case-string msg
	       (("north" "n") "north")
	       (("south" "s") "south")
	       (("east" "e") "east")
	       (("west" "w") "west")
	       (("down" "d") "down")
	       (("up" "u") "up")))


(defun from-paths (path-str)
  (case-string
   path-str
   ("north" "the south")
   ("south" "the north")
   ("east" "the west")
   ("west" "the east")
   ("up" "below")
   ("down" "above")))

(defun from-path (path-str)
  (case-string
   path-str
   ("north" "south")
   ("south" "north")
   ("east" "west")
   ("west" "east")
   ("up" "down")
   ("down" "up")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun load-pools ()
  (setq pools (read-things-file 'pool "pools.sexp")))
(load-pools)
