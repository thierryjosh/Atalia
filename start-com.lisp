;;;;
;;;;  communication : any code to any code anywhere
;;;;  https://github.com/orivej/pzmq
;;;;  https://www.quicklisp.org/beta/faq.html
;;;;  https://40ants.com/lisp-project-of-the-day/2020/10/0206-pzmq.html
;;;;  
;;;;  (defparameter *server-thread* (bt:make-thread #'hwserver))
;;;;  
;;;;
(defglobal *genval-in* nil)
(defglobal *genval-out* nil)
(defglobal *genvalx* nil)
(defglobal *genstack-req* nil)
(defglobal *gendebug-flag* nil)

(declaim (ftype function exec-token-group))
;;;;-----------------------------------------------------------------------------
;;;;  basic req-rep pattern

(defun test-hwserver (&optional (listen-address "tcp://*:5555"))
  "Translation of http://zguide.zeromq.org/c:hwserver updated for ZMQ 3. "
  (pzmq:with-context nil ; use *default-context*
    (pzmq:with-socket responder :rep
      (pzmq:bind responder listen-address)
      (loop
        (write-string "Waiting for a request... ")
        (write-line (pzmq:recv-string responder))
        (sleep 1)
         (pzmq:send responder "World")))))

(defun test-hwclient (&optional (server-address "tcp://localhost:5555"))
  "Translation of http://zguide.zeromq.org/c:hwclient updated for ZMQ 3.  Includes some parameters in with-* macros to demonstrate syntax."
  (pzmq:with-context (ctx :max-sockets 10)
    (pzmq:with-socket (requester ctx) (:req :affinity 3 :linger 100)
      ;; linger is important in case of (keyboard) interrupt;
      ;; see http://api.zeromq.org/3-3:zmq-ctx-destroy
      (write-line "Connecting to hello world server...")
      (pzmq:connect requester server-address)
      (dotimes (i 10)
        (format t "Sending Hello ~d...~%" i)
        (pzmq:send requester "Hello")
        (write-string "Receiving... ")
        (write-line (pzmq:recv-string requester))))))

;;;;-----------------------------------------------------------------------------
;;;; simple req-rep for spores

(defun spores-srv (&optional (listen-address "tcp://*:5555"))
 (let ((xs nil))
  (pzmq:with-context nil ; use *default-context*
    (pzmq:with-socket responder :rep
      (pzmq:bind responder listen-address)
      (loop
         (write-line "Waiting for a request... ")
	 (setf xs (pzmq:recv-string responder))
	 (setf *genval-out* nil)
	 (exec-token-group xs)
	 (write-line xs)
	 ;(setf zr (format nil "~a" genval1))
         (if (equal *genval-out* nil)(setf *genval-out* "*"))
         (write-line *genval-out*)
         (sleep 1)
	 
         (pzmq:send responder *genval-out*))))
   )
  )

(defun spores-cli (&optional (server-address "tcp://localhost:5555"))
 (let ((xs nil))
  (pzmq:with-context (ctx :max-sockets 10)
    (pzmq:with-socket (requester ctx) (:req :affinity 3 :linger 100)
      ;; linger is important in case of (keyboard) interrupt;
      ;; see http://api.zeromq.org/3-3:zmq-ctx-destroy
      (write-line "Connecting to server...")
      (pzmq:connect requester server-address)
      (dotimes (i 1)
        (format t "Sending ...  ~d...~%" i)
	(terpri)
	(setf *genval-in* nil)
        ;(pzmq:send requester (concatenate 'string "(setf genval1 (enum-next 5" "\"W" "\"" "\"Ke1" "\"))"))
        ;(pzmq:send requester (enum-next-dist 5 "W" "Ke1"))
	;(pzmq:send requester (load-cluster-dist "load-chess-nn" "/home/arkho/.emacs.d/chess-atalia-nn19.lisp"))
	;(pzmq:send requester (eval-nextmove-nn-dist *c-seg-ply*))
	(setf xs (pop *genstack-req*))
	(if (not (equal xs nil))
            (progn
	      (write-line xs)
	      (pzmq:send requester xs)
	      ))
	(write-line "Receiving... ")
	(setf *genval-in* (pzmq:recv-string requester))
        (write-line *genval-in*)
	)))
  )
 )

;;;;-----------------------------------------------------------------------------
;;;; Lazy pirate pattern

(defun srv-lazy-pirate (&optional (listen-address "tcp://*:5555"))
 (let ((xs nil))
  (pzmq:with-context nil ; use *default-context*
    (pzmq:with-socket responder :rep
      (pzmq:bind responder listen-address)
      (loop
         (write-line "Waiting for a request... ")
	 (setf xs (pzmq:recv-string responder))
	 (setf *genval-out* nil)
	 (exec-token-group xs)
	 (write-line xs)
         (if (equal *genval-out* nil)(setf *genval-out* "*"))
         (write-line *genval-out*)
         (sleep 1)
	 
         (pzmq:send responder *genval-out*))))
   )
  )

(defun cli-lazy-pirate (&optional (server-address "tcp://localhost:5555"))
  (let ((xs nil)(retries-nb 0)(rc nil)(i 0)(s1 nil))
    (setf *genval-in* nil)
    (setf xs (pop *genstack-req*))
    (loop while (< retries-nb 3)
       do 
         (pzmq:with-context (ctx :max-sockets 10)
           (pzmq:with-socket (requester ctx) (:req :affinity 3 :linger 100)
	     (if (not (equal  *gendebug-flag* nil))(write-line "Connecting to server..."))
             (pzmq:connect requester server-address)
             (setf retries-nb (+ retries-nb 1))	     
	     (if (not (equal xs nil))
                 (progn
		   (if (not (equal  *gendebug-flag* nil))(write-line "Sending ..."))
	           (if (not (equal  *gendebug-flag* nil))(write-line xs))
	           (pzmq:send requester xs)
		   (if (not (equal  *gendebug-flag* nil))(write-line (concatenate 'string "Expecting reply ...  " (write-to-string retries-nb))))
		   (setf i 0)
		   (setf s1 "No response from server ... ")
                   (pzmq:with-poll-items items (requester)			 
                     (loop
			(setf i (+ i 1))
			(if ( > i 1000)(progn
                                       (setf retries-nb 3)
				       (return)
				       ))
                         (setf rc (pzmq:poll items 3000))    ;timeout 3000 ms
			 (if (not (equal  *gendebug-flag* nil))(write-line (concatenate 'string "termination code = " (write-to-string rc))))	
			 (if (=  rc -1)(progn    ;-1 failure  0 no event
                                         (setf s1 "Network failure ... retrying ... ")
					 (return)
					 ))
			  (when (member :pollin (pzmq:revents items 0))    ;0 index of first in neme list  to poll in poll-items
                              (if (not (equal  *gendebug-flag* nil))(write-line "Server replied ... "))
	                      (setf *genval-in* (pzmq:recv-string requester))
                              (if (not (equal  *gendebug-flag* nil))(write-line *genval-in*))
			      (setf retries-nb 3)
		              (return-from cli-lazy-pirate T)
			  )
		       ))
                    (if (not (equal  *gendebug-flag* nil))(write-line s1))		   
	           )
		 (progn
                   (setf retries-nb 3)
		   (return-from cli-lazy-pirate nil)
		   ))
              ))
       )
    (write-line "Abandoning ...")
    (return-from cli-lazy-pirate nil)
  )
 )

;(sb-thread:thread-name SB-THREAD:*CURRENT-THREAD*)
;(sb-thread:list-all-threads)
;(sb-thread:make-thread (lambda ()) :name "good thread")
(defun lambda-lazy-pirate ()
  (let ((rth nil))
    (setf rth (sb-thread:make-thread (lambda () (cli-lazy-pirate))))
    (return-from lambda-lazy-pirate rth)
    )
  )

