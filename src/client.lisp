(in-package :mush)

(defparameter terminal-width 60)

(defun send (target str)
  (unwind-protect
       (handler-case 
	   (progn (setq soul nil)	     
	     (if (typep target 'soul)
		 (setq soul target
		       target (socket-stream (conn target))))
	     (format target str)
	     (terpri target)
	     (force-output target))
	 (SB-BSD-SOCKETS:NOT-CONNECTED-ERROR ()
	   (print "not-connected") (clean-soul soul))
	 (SB-INT:CLOSED-STREAM-ERROR ()
	   (print "closed-stream") (clean-soul soul))
	 (SB-INT:SIMPLE-STREAM-ERROR ()
	   (print "broken-pipe-error") (clean-soul soul))
	 )))

(defun clean-soul (soul)
  (if soul (setf (things (ghost-pool soul))
		 (remove soul (things (ghost-pool soul)) :test #'ichp))))

(defun send-bar (stream length)  
  (send stream
	(c+ "=" (coerce (loop for x from 0 upto length collect #\-) 'string) "=")))
    

(defun newline (conn-stream &optional times)
  (send conn-stream "")
  (if times (loop for i from 2 upto times do
		 (send conn-stream ""))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (User Command Loop)
;;

(defun welcome-connection (stream)
  (send-lines
   stream
   '("" "" "" "" ""
     "The soul leaves the body..."     
     )))

(defun welcome-soul (conn stream user)
  (send-lines
   stream	   
   '(""))       
  (setf (soul user) (new-soul conn))
  (look (soul user) (ghost-pool (soul user)))
  (soul user))

(defun handle-user (user conn stream)
  (let* ((soul (soul user))
	 (msg (remove #\Newline (remove #\Return (read-all stream)))))

    ;; soul Reincarnate
    (if (null soul)
	(progn (setq soul (welcome-soul conn stream user))
	       (push soul (things (ghost-pool soul)))))
    (user-cmd user conn stream soul msg)
    ))


(defun user-cmd (user conn stream soul msg)      
  (print `(Saith ,soul ,msg))
  (if (equal msg "quit")
      (progn (close-connection conn) (clean-soul soul))
      (progn
	(if (and soul (not (equal msg "")))
	    (being user conn stream soul msg))
	)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Soul Being)
;;


(defun being (user conn stream soul msg)
  (let* ((pool (ghost-pool soul))
	 (split-msg (cl-ppcre:split " " msg))
	 (rest-msg (join (rest split-msg) " "))
	 (doing (string-downcase (first split-msg))))
    
    (case-string doing
     
     (("look" "l")
      (if (second split-msg)
	  (look-at user conn stream soul pool (second split-msg))
	  (look soul pool)))     
     (("chat" "c") (ooc-chat user conn stream soul rest-msg))
     (("history" "his") (chat-history user conn stream soul))
     (("say" "s") (say soul rest-msg))
     (("emote" "e") (emote soul rest-msg))     
     ("iknow" (iknow soul))
     ("pmake" (pmake soul (second split-msg)))
     ("path" (path soul pool (second split-msg) (third split-msg)))
     ("phight" (phight soul pool rest-msg))
     ("psight" (psight soul pool rest-msg))
     (("help" "h" "?") (help soul))
     (("who" "online") (who soul))
     ("!" (being user conn stream soul (last-doing user)))
     )

    (if (>= (know soul) 7)
	(case-string doing
	 ("goto" (goto soul (second split-msg)))
	 ("pools" (pools-list soul))
	 ))    
    (movement-check user conn stream soul pool msg)
    (action-check soul pool doing)
    
    (if (not (equal doing "!"))
	(setf (last-doing user) doing))
  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Soul Doings)
;;


(defun sendall (msg)
  (loop for conn in connections
     if (not (typep conn 'stream-server-usocket)) do	     
       (send (socket-stream conn) msg)))
	     
  
(defun ooc-chat (user conn stream soul msg)
  (push msg message-history)
  (loop for conn in connections
     if (not (typep conn 'stream-server-usocket)) do	     
       (send (socket-stream conn)
	     (c+ (sight soul) " saith, " msg))))


(defun chat-history (user conn stream soul)
  (if (null message-history)
      (send soul "No chat messages said since server message log startup.")
      (loop for msg in message-history do
	   (send soul (c+ "someone said : '" msg "'")))))


(defun look (soul pool)
  (newline soul 3)    
  (send soul (line-wrap (sight pool) terminal-width))
  
  (newline soul 1)
  (loop for path-str in (path-strs pool) do
       (send soul (c+ path-str " - "
		      (hight (pool (second (assoc path-str (paths pool))))))))
  
  (newline soul 1)
  
  (loop for thing in (things pool) 
     if (not (ichp thing soul)) do
       (send soul (rsight thing)))
  (newline soul 2)
  )

(defun look-at (user conn stream soul pool target)
  (newline soul)
  (loop for thing in (things pool) do
       (cond ((member target (keys thing) :test #'equal)
	      (emote soul (c+ "looks at " (hight thing)))
	      (send soul (line-wrap (sight thing) terminal-width))	      
	      (cond ((actions thing)
		     (send soul
			   (c+ "You can "
			       (join-and (mapcar #'string-upcase
						 (mapcar #'car (actions thing)))) " it."))))	      
	      ))))


(defun movement-check (user conn stream soul pool msg)
  (loop for path-str in (path-strs pool)
     if (equal (path-match msg) path-str) do
       (send soul (c+ "You go " path-str))
       (move soul path-str)
       (look soul (ghost-pool soul))))


(defun action-check (soul pool action)
  (loop for thing in (things pool) do
       (let* ((action-sexp (assoc action (actions thing) :test #'equal))
	      (action-lambda (eval (third action-sexp)))
	      (action-verb (second action-sexp)))
	 (cond (action-sexp
		(funcall action-lambda thing pool soul)
		(emote soul (c+ action-verb " " (hight thing))))
	       ))))
			        
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Help)
;;

(defun who (soul)
  (setq who-str
	(c+ "There are "
	    (write-to-string (length (rest connections)))
	    " souls connected including you."))
  ;; (loop for conn in connections
  ;;    if (not (typep conn 'stream-server-usocket)) do	     
  ;;      (send (socket-stream conn)
  ;; 	     (c+ (sight soul) " saith, " msg)))
  (send soul who-str))
  

(defun help (soul)
  (newline soul)
  (send-lines
   soul
   (list "look, l"
	 "look <thing>"
	 "chat, c <global-message>"
	 "history, his (returns global message log for server session)"
	 "who (checks other connected peers)"
	 "say, s <local message in a room>"
	 "emote, e <local act in a room>"
	 "north, east, west, south, up, down, n, e, w, s, u, d"
	 "help, h, ?"
	 "! (repeats last action)"
	 "quit"
	 )))

(defun know-help (soul)
  (send-lines
   soul   
   '("pools"
     "goto 4"
     "path west 4"
     "pmake 7")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Knowing)
;;

(defun iknow (soul)
  (send soul "Thou knowst.")
  (setf (know soul) 14))


(defun goto (soul poolid-str)
  (if poolid-str
      (let ((poolid (parse-integer poolid-str)))
	(teleport soul poolid)
	(look soul (pool poolid)))))
      

(defun teleport (ghost poolid)
  (let* ((old-room (pool (poolid ghost)))
	 (new-room (pool poolid)))
    (setf (things old-room)
	  (remove ghost (things old-room) :test #'ichp))
    (push ghost (things new-room))
    (setf (poolid ghost) poolid)))


(defun pools-list (soul)  
  (loop for pool in (sort pools (lambda (x y) (< (id x) (id y)))) do
       (send soul (c+ (id pool) " " (hight pool)))))


(defun pmake (soul poolid-str)
  (ignore-errors
    (let ((poolid (parse-integer poolid-str)))
      (cond ((not (pool poolid))
	     (push (make-instance 'pool
				  :id poolid
				  :hight "a padded cell"
				  :sight "Unlit save for a dim night light behind a gel cube embedded in the corner, the wide mattressed floor is enclosed by cushioned walls rising high towards an unseen ceiling. The cushions and air of this room smell sweetly sterile.")
		   pools)
	     (write-pools))
	    ((pool poolid) (send soul (c+ "Pool " poolid-str " already made.")))
	    ))))

(defun path (soul pool path poolid-str)
  (if (null poolid-str)
      (setf (paths pool) (remove-if (lambda (p) (equal (first p) (path-match path))) (paths pool)))
      (let* ((poolid (parse-integer poolid-str))
	     (to-pool (pool poolid))
	     (path (path-match path)))
	(cond ((not (pool poolid))
	       (send soul (c+ "Pool " poolid-str " isn't made.")))
	      ((pool poolid)
	       (setf (paths pool) (remove-if (lambda (p) (equal (first p) path)) (paths pool)))
	       (push (list path poolid) (paths pool))
	       (setf (paths to-pool) (remove-if (lambda (p) (equal (first p) (from-path path))) (paths to-pool)))
	       (push (list (from-path path) (id pool)) (paths to-pool))
	       ))))
  (write-pools))


(defun phight (soul pool hight)
  (setf (hight pool) hight)
  (write-pools)
  (look soul pool))


(defun psight (soul pool sight)
  (setf (sight pool) sight)
  (write-pools)
  (look soul pool))

      
