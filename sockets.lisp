;;;;
;;;; sockets
;;;;
;;;;https://github.com/usocket/usocket/blob/master/backend/sbcl.lisp
;;;;
;;;; https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html
;;;;
;;;;https://developer.mozilla.org/en-US/docs/Web/HTTP/Messages
;;;;
(defun test01-sockets ()
  (let ((sk nil)(sh nil)(sd nil)(fl nil))
    (setf sk (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
    ;(setf sh (make-instance 'sb-bsd-sockets:host-ent :name :adresses))
    (setf sh (sb-bsd-sockets:get-host-by-name "arkho.net"))
    (describe sh)
    ;(slot-value sh  'addresses)
    (setf sd (sb-bsd-sockets:host-ent-addresses sh))
    ;(format t "~a" sd)
    ;(princ (elt sd 0))
    (terpri)
    (sb-bsd-sockets:socket-connect sk (elt sd 0) 443)
    (setf fl (sb-bsd-sockets:socket-make-stream sk :output t :input t))
    (format fl "~A~C~C~A~C~C~C~C"
                                        "GET /cooddd/index.php  HTTP/1.1"
                                        #\Return #\Newline
                                        "Host: www.arkho.net"
                                        #\Return #\Newline
                                        #\Return #\Newline)
    
    (force-output fl)

    (do ((line                                                            
         (read-line fl nil)                       
         (read-line fl nil)))                     
       ((not line))                                                      
      (format t "~A" line))
    
    (sb-bsd-sockets:socket-close sk)
    (return-from test01-sockets (values sk sh))
    )
  )
