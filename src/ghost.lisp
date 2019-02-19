
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


(defun bob ()
  (make-instance
   'ghost
   :hight "Daddybob"
   :poolid 4
   :ich (uuid-integer)
   :rsight "an old man walking"
   :sight "Patriarch of the Nichols family."
   :keys '("man" "old" "daddybob" "bob")))

(defun alvar ()
  (make-instance
   'ghost
   :hight "pale elf"
   :poolid 905
   :ich (uuid-integer)
   :rsight "a young pale elf bathing in the pool"
   :sight "A lithe pale grey-skinned Alvar washes their naked body, standing in a shallow pool as hot water pours from a cave stream. It has long thin forearms, a thin chest, and taut thighs girdling a long flacid penis, black eyes look out under locks of feather silver hair."
   :keys '("elf" "pale" "alv")))

(defun naiad ()
  (make-instance
   'ghost
   :hight "a cave naiad"
   :poolid 914
   :ich (uuid-integer)
   :rsight "a teal naiad girl wearing a yellow bikini"
   :sight "The young naiad girl has taut teal skin with the exception of her face and neck which shifts from her normal blue gradient to a natural human skin toned face with blonde hair and jagged stripes at the throat. She stands by the shelf of pool toys, lifting heavy water balloons to her lips and drinking the water, inflating her thighs, and hips, shifting water weight from one cheek to the other. The naiad giggles as her blue nipples slowly engorge against her tiny yellow bikini top, her growing hips and ass swaying in protest against her thong strap as she playfully adjustes her elastic curves. She bends over a yoga mat, sucking water from a hose in gulps as her breasts swell to the size of beach balls as her belly begins to slosh like an enormous water balloon."
   :keys '("cave" "naiad" "teal" "yellow" "bikini")))


(defun spawn-ghosts ()
  ;;(spawn (bob) (pool 4))
  ;; (spawn (alvar) (pool 905))
  ;;  (spawn (naiad) (pool 914))
  )
(spawn-ghosts)		 
