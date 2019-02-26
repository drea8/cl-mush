
(in-package :mush)

;;;;;;;;;;;;;;;;;;;;;;;;
;; (Ghosts)
;;

(defthing ghost (thing))

(defun ghost-pool (ghost)
    (pool (poolid ghost)))

(defun move (ghost path-str)
  (let ((old-room (pool (poolid ghost)))
	(new-room (path-pool (ghost-pool ghost) path-str)))
    (emote ghost (c+ "goes " path-str))
    (setf (things old-room)
	  (remove ghost (things old-room) :test #'ichp))
    (push ghost (things (pool new-room)))
    (setf (poolid ghost) new-room)
    (emote ghost (c+ "comes from " (from-paths path-str)))
    ))
	

(defun spawn (thing pool)
  (set-pool thing (id pool))
  (push thing (things pool)))


(defun emote (thing emote)
  (let ((pool (pool (poolid thing))))
    (loop for soul in (souls-pool pool)
	 if (not (ichp soul thing))
       do (send soul
		(c+ (hight thing) " " emote)))))

(defun say (thing saying)
  (emote thing (c+ "says '" saying "'")))


;;;;;;;;;;;;;;;;;;;;;;;;
;; (Ghost Actor Model)
;; ;; TO DO
;; '(register triggers
;;   -> check wildcard user command against triggers
;;   -> change state)
;; ;;


(defun register-trigger (ghost trigger)
  )


(defun oyestikkere ()
  (make-instance
   'ghost
   :hight "oyestikke"
   :poolid 4
   :ich (uuid-integer)
   :rsight "en Oyestikke flitte har"
   :sight "en Oyestikke flitte har"
   :keys '("oyestikkee" "oye")))


(defun zample ()
  (make-instance
   'ghost
   :hight "zample"
   :poolid 4
   :ich (uuid-integer)
   :rsight "en zample"
   :sight "en zample"
   :keys '("zample")
   
   :Aktyr '()   
   )



(defun spawn-ghosts ()
  ;;(spawn (bob) (pool 4))
  )
(spawn-ghosts)		 
