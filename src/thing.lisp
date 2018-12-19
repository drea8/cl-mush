
(in-package :mush)

(defthing thing ()
  hight
  ich
  poolid
  rsight
  sight
  keys
  actions
  triggers 
  state states)

(defun ichp (x y)
  (equal (ich x) (ich y)))



(defun spawn (thing pool)
  (set-pool thing (id pool))
  (push thing (things pool)))

(defun set-pool (ghost poolid)
  (setf (poolid ghost) poolid))


;;;;;;;;;;;;;;;;;;;;;;;

(defparameter things '() )


(defmacro make-thing (symbol &rest slots)
  `(progn     
     (if (or (not (boundp (quote ,symbol))) (not (typep ,symbol 'thing)))
	 (progn (setq ,symbol (make-instance 'thing))
		(setf (ich ,symbol) (uuid:make-v4-uuid))))
     (if (not (member ,symbol things :test #'ichp))
	 (push ,symbol things))
     ,@(loop for i from 0 upto (1- (length slots)) collect
	    `(setf (,(intern (symbol-name (nth i slots))) ,symbol) ,(nth (incf i) slots)))))

		     
(make-thing brochure
   :hight "a brochure"
   :poolid 4   
   :rsight "a brochure is on the ground"
   :sight "The front of the brochure shows a man sobbing in the arms of a smiling elderly woman in a garden. On the back superimposed over a photo of the ocean it reads: ~% ~% 'In 1998, Esalen launched the Center for Theory and Research to initiate new areas of practice and action which foster social change and realization of the human potential. It is now the head research and development arm of Esalen Institute. Michael Cornwall, previously in the Institutes' Schizophrenia Research Project at Agnews State Hospital, conducts workshops titled the Alternative Views and Approaches to Psychosis Initiative at Esalen, inviting leaders in the field of psychosis treatment to attend the workshops.'"
   :keys '("brochure"))


(make-thing sea-urchin
   :hight "a sea urchin"
   :rsight "a sea urchin lays in the tide pool"
   :sight "In a deep puddle in the rocks floating over a patch of algae, the purple spines drift about gently with each ebb in the pool."
   :poolid 4   
   :actions '(("touch" "touches"
	       (lambda (thing pool soul)
		 (emote thing "crawls under a rock in the tide pool"))))
   :keys '("sea" "urchin"))
   

(make-thing tape
   :hight "a tape recorder"
   :rsight "a tape recorder is here"
   :sight "A black tape recorder."
   :poolid 8
   :actions '(("play" "plays"
	       (lambda (thing pool soul)
		 (emote thing "plays a low flat sawtooth tone for a few seconds"))))
   :keys '("tape" "recorder"))
   



(defun spawn-things ()
  ;;(spawn brochure (pool 4))
  ;;(spawn sea-urchin (pool 4))
  ;;(spawn tape (pool 8))
  )

(setf (Things (pool 4)) '())
(spawn-things)
