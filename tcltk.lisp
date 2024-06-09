;; Copyright (c) 2019 Josh Cimers
;; Copyright (c) 2008-2014 Paul Griffioen
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; ----------------------------------------------------------------------------
;; Package definition for the clTcl package
;; 
;; Paul Griffioen 2008-2014
;; ----------------------------------------------------------------------------

(in-package :cl-user)

(defpackage :cltcl
  (:use :cl :cl-user)
  (:export
   "READ-SCRIPT"
   "READ-LIST"
   "READ-WORD"
   "FORMAT-SCRIPT"
   "ESCAPE"

   "WITH-TCL/TK"
   "OPEN-TCL/TK-STREAM"
   "CLOSE-TCL/TK-STREAM"
   "SEND-SCRIPT"
   "RECEIVE-LINE"

   "*INTERPRETER*"
   "*STREAM*"
   "*TRACE-LEVEL*"
   "*DEBUG*"
   "EVENT-LOOP"
   "RUN"
   "CALL"
   "POST"
   "READ-TCL-LIST-FROM-STRING1"
   "WRITE-LIST-TO-TCL-STRING"
   "KEEP-LISTENING"
   
   "REPL"))


(in-package :cltcl)

(defun READ-WHITE-SPACE (STREAM)
  "Reads white space from STREAM, except newlines."
  (with-output-to-string (s)
    (loop
       for next = (peek-char nil stream nil)
       while (member next '(#\space #\tab))
       do (write-char (read-char stream) s))))

(defun READ-COMMENT (STREAM)
  "Reads a line of Tcl comment from STREAM but leaves the newline."
  (with-output-to-string (s)
    (loop for char = (peek-char nil stream)
	 until (member char (list #\return #\newline))
	 do (write-char (read-char stream) s))))

(defun READ-BRACED (STREAM &OPTIONAL ECHO-P)
  "Reads a Tcl braced expression from STREAM."
   (with-output-to-string (s)
     (loop 
	initially (let ((brace (read-char stream)))
		    (when echo-p (write-char brace s)))
	for next = (peek-char nil stream)
	until (eql next #\})
	do (cond ((eql next #\{)
		  (write-string (read-braced stream t) s))
		 ((eql next #\\)
		  (let ((next-char (read-char stream)))
		    (when (and (not echo-p)
			       (eql next-char #\newline))
		      'todo) ;checken dmv repl
		    (write-char next-char s))
		  (write-char (read-char stream) s))
		 (t (write-char (read-char stream) s)))
	finally (let ((brace (read-char stream)))
		  (when echo-p (write-char brace s))))))

(defun WRITE-ESCAPED (X STREAM)
  "Escapes all characters that have a special meaning for
Tcl (includes whitespace) with a backslash."
  (with-input-from-string (s x)
    (loop for ch = (read-char s nil)
       while ch do
       (when (member ch '(#\newline #\return
			  #\space #\tab
			  #\\ #\$ #\; #\"
			  #\[ #\]
			  #\{ #\}))
	 (write-char #\\ stream))
       (write-char (case ch
		     (#\Backspace #\b)
		     (#\Page #\f)
		     (#\Newline #\n)
		     (#\Return #\r)
		     (#\Tab #\t)
		     (t ch))
		   stream))))

(defun WRITE-ESCAPED-CHAR (CHAR STREAM)
  "Writes character CHAR to stream. Special Tcl escape characters
\\b, \\f, \\r,\\n and \\t are converted."
  (write-char (case char
		(#\b #\Backspace)
		(#\f #\Page)
		(#\n #\Newline)
		(#\r #\Return)
		(#\t #\Tab)
		(t char))
	      stream))

(defun READ-DOUBLE-QUOTED (STREAM &OPTIONAL ECHO-P)
  "Reads a Tcl double quoted expression from STREAM."
  (with-output-to-string (s)
    (loop initially
	 (let ((quote (read-char stream)))
	   (when echo-p (write-char quote s)))
	 for char = (read-char stream)
	 until (eql char #\")
	 do (if (eql char #\\)
		(let ((next-char (read-char stream)))
		  (if echo-p
		      (progn (write-char char s)
			     (write-char next-char s))
		      (write-escaped-char next-char s)))
		(write-char char s))
	 finally (when echo-p (write-char char s)))))

(defun ESCAPE (X)
  "Escapes all characters that have a special meaning for
Tcl (includes whitespace) with a backslash."
  (with-output-to-string (stream)
    (write-escaped (if (stringp x) x (princ-to-string x))
		   stream)))


(defun CLEAN-SCRIPT (SCRIPT)
  "Removes all empty strings and comments from command list SCRIPT."
  (loop
     for line in script
     unless (or (string-equal line "")
                (char= (aref line 0) #\#))
     collect line))

;;how to declare mutually recursive functions  --> to avoid undefined function
(declaim (ftype function read-word))

(defun READ-LIST (&OPTIONAL (STREAM *STANDARD-INPUT*) TERMINATOR)
  "Reads a Tcl list from STREAM. Reads Tcl words until an end of file
occurs or READ-WORD finds TERMINATOR (when given), a semi-colon or a
newline. The Tcl list is returned as a string. The default value for
STREAM is *STANDARD-INPUT*. Returns NIL if end of file is found
immediately."
  (read-white-space stream)
  (let ((char (peek-char nil stream nil)))
    (cond ((null char) nil)
	  ((eql char #\#) (read-comment stream))
	  (t (with-output-to-string (s)
	       (loop
		  for word = (read-word stream t t t terminator t)
		  while word do 
		    (write-string word s)
		    (write-string (read-white-space stream) s)))))))

(defun READ-SCRIPT (&OPTIONAL (STREAM *STANDARD-INPUT*) TERMINATOR)
  "Reads a Tcl script from STREAM. Reads Tcl commands until READ-LIST
finds TERMINATOR (when given) or an end of file occurs. Returns the
commands as a list of strings. The default value for STREAM is
*STANDARD-INPUT*."
  (loop 
     for command = (read-list stream terminator)
     for char = (read-char stream nil)
     when command collect command
     until (or (null char)
	       (eql char terminator))))

(defun READ-WORD-AUX (STREAM EOF-ERROR-P TERMINATOR ECHO-P)
  "Helper for READ-WORD."
  (with-output-to-string (s)
    (loop
       for next = (peek-char nil stream eof-error-p)
       until (or (null next)
		 (when terminator
		   (eql next terminator))
		 (member next '(#\space #\tab #\return #\newline #\;)))
       do (let ((char (read-char stream)))
	    (cond ((eql char #\[)
		   (format s "[~{~A~^~%~}]" (read-script stream #\])))
		  ((eql char #\\)
		   (let ((next-char (read-char stream)))
		     (if echo-p
			 (progn (write-char char s)
				(write-char next-char s))
			 (write-escaped-char next-char s))))
		  (t (write-char char s)))))))


(defun READ-WORD (STREAM &OPTIONAL
		   (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P TERMINATOR ECHO-P)
  "Reads a Tcl word from STREAM and returns it as a string. Reads
characters untill the next one is a terminator. The terminator itself
is not read. Terminators are TERMINATOR (when given), whitespace or
Tcl list terminators. Throws an error if an end of file occurs, unless
EOF-ERROR-P is nil, in which case it returns EOF-VALUE. If ECHO-P is
non nil then escape characters are not handled and the word is
literally copied."
  (read-white-space stream)
  (let ((char (peek-char nil stream eof-error-p recursive-p))) ;; is recursive-p okay?
    (cond ((null char) 
	   eof-value)
	  ((member char (list terminator #\; #\return #\newline))
	   nil)
	  ((eql char #\")
	   (read-double-quoted stream echo-p))
	  ((eql char #\{)
	   (read-braced stream echo-p))
	  (t (read-word-aux stream eof-error-p terminator echo-p)))))


(defun READ-TCL-LIST-FROM-STRING1 (STRING)
  "Converts Tcl list STRING into a list of strings. Items are
delimited by whitespace."
  (with-input-from-string (stream string)
    (loop 
       for word = (prog1 (read-word stream nil)
		    (read-white-space stream))
       while word collect word)))

(defun WRITE-TCL-STRING (OBJECT &OPTIONAL (STREAM *STANDARD-OUTPUT*) RECURSIVEP)
  "Converts list to a Tcl string. Nested lists are converted
recursively. Strings are escaped. Other values are written to string
and escaped." 
  (typecase object
    (list (loop
	     initially (when recursivep
			 (write-char #\{ stream))
	     for (item . rest) on object
	     do (write-tcl-string item stream t)
	     while rest
	     do (write-char #\space stream)  
	     finally (when recursivep
		       (write-char #\} stream))))
    (string (if (and recursivep (equal object ""))
		(princ "{}" stream)
		(write-escaped object stream)))
    ;;(number (write (floor object) :stream stream))
    (float (format stream "~,10,F" object))
    (t (write-escaped (princ-to-string object) stream))))

(defun WRITE-LIST-TO-TCL-STRING (LIST)
  "Converts list to a Tcl string. Nested lists are converted
recursively. Strings are escaped. Other values are written to string
and escaped." 
  (with-output-to-string (s)
    (write-tcl-string list s)))

(defun FORMAT-SCRIPT (SCRIPT &REST ARGS)
  "Applies function FORMAT to SCRIPT's commands sequentially. Each
element consumes its required formatter arguments from ARGS, leaving
the rest of the arguments for the rest of the commands. For clTcl
script written with #TCL[...] this gives the effect of a single format
on the entire Tcl code."
  (mapcar (lambda (x)
	    (with-output-to-string (s)
	      (setf args (apply (formatter "~@?") s x args))))
	  script))

(set-dispatch-macro-character
 #\# #\T 
 (lambda (stream char i)
   (declare (ignore char i))
   (assert (and (eql (read-char stream) #\C)
		(eql (read-char stream) #\L)
		(eql (read-char stream) #\[))
	   ()
	   "Invalid clTcl, use #TCL[...]")
   (list 'quote (read-script stream #\]))))


;;-----------------------------------------------------------------------------
;; clTcl communication
;;
;; 0. The with-Tcl/Tk macro
;; 1. Connecting to Tcl/Tk
;; 2. Communicating with Tcl/Tk
;;
;;-----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; 0. The with-Tcl/Tk macro
;; ----------------------------------------------------------------------------

(defmacro WITH-TCL/TK ((VAR &REST ARGS) &BODY BODY)
  "Uses OPEN-TCL/TK-STREAM to start a Tcl/Tk interpreter and connect
to it by a two-way stream. Arguments ARGS are used as keyword
arguments to OPEN-TCL/TK-STREAM. The forms in BODY are evaluated as an
implicit progn with VAR bound to the stream returned by
OPEN-TCL/TK-STREAM. When control leaves the body, either normally or
abnormally, the stream is closed with CLOSE-TCL/TK-STREAM."
  `(let ((,var (open-Tcl/Tk-stream . ,args)))
     (unwind-protect (prog1 (progn ,@body)
		       (close-tcl/tk-stream ,var))
       (when (open-stream-p ,var)
	 (send-script ,var #TCL[exit])
	 (close-tcl/tk-stream ,var)))))

;; ----------------------------------------------------------------------------
;; 1. Connecting to Tcl/Tk
;; ----------------------------------------------------------------------------

(defun OPEN-TCL/TK-STREAM (&KEY INTERPRETER OPTIONS)
  "Starts a Tcl/Tk interpreter and creates and returns a two-way
stream connected to this Tcl/Tk process. This function and the meaning
of arguments INTERPRETER and OPTIONS are implementation dependent."
  (handler-case
      #+:CLISP (let ();;(custom:*default-file-encoding* :iso-8859-1))
		 (ext:run-program interpreter
				  :arguments options
				  :input     :stream
				  :output    :stream))
      #+:LUCID (lcl:run-program interpreter
		 :arguments options
		 :input     :stream
		 :output    :stream
		 :wait      NIL)
      #+:ALLEGRO (excl:run-shell-command
		     (format NIL "exec ~A~{ \"~A\"~}" 
		      interpreter options)
		   :input        :stream
		   :output       :stream
		   :wait         NIL)
      #+:sbcl (let ((process (sb-ext:run-program interpreter options
						 :input :stream
						 :output :stream
						 :wait nil)))
		(make-two-way-stream (sb-ext:process-output process)
				     (sb-ext:process-input process)))
      #+:abcl (let ((lispobject-class (java:jclass "org.armedbear.lisp.LispObject"))
		    (symbol-class (java:jclass "org.armedbear.lisp.Symbol"))
		    (inputstream-class (java:jclass "java.io.InputStream"))
		    (outputstream-class (java:jclass "java.io.OutputStream"))
		    (process-class (java:jclass "java.lang.Process")))
		(let ((SYSTEM_STREAM (java:jfield symbol-class "SYSTEM_STREAM"))
		      (CHARACTER (java:jfield symbol-class "CHARACTER")))
		  (let ((process (let ((Runtime (java:jclass "java.lang.Runtime")))
				   (java:jcall (java:jmethod Runtime "exec"
							     (java:jclass "java.lang.String"))
					       (java:jcall (java:jmethod Runtime "getRuntime")
							   Runtime)
					       interpreter))))
		    (let ((input-stream (java:jnew (java:jconstructor 
						    "org.armedbear.lisp.Stream" 
						    symbol-class
						    inputstream-class
						    lispobject-class)
						   SYSTEM_STREAM
						   (java:jcall (java:jmethod process-class
									     "getInputStream")
							       process)
						   CHARACTER))
			  (output-stream (java:jnew (java:jconstructor 
						     "org.armedbear.lisp.Stream" 
						     symbol-class 
						     outputstream-class
						     lispobject-class)
						    SYSTEM_STREAM
						    (java:jcall (java:jmethod process-class
									      "getOutputStream") 
								process)
						    CHARACTER)))
		      (make-two-way-stream input-stream output-stream)))))
      #+:cmu (let ((process (ext:run-program interpreter options
					     :input :stream
					     :output :stream
					     :wait nil)))
	       (make-two-way-stream (ext:process-output process)
				    (ext:process-input process)))
      #+:ccl (let ((process (ccl:run-program interpreter options
					     :input :stream
					     :output :stream
                                             :external-format :utf-8 ;;:iso-8859-1 ;; :L1
					     :wait nil)))
	       (make-two-way-stream 
		(ccl:external-process-output-stream process)
		(ccl:external-process-input-stream process)))
      #+:LispWorks (system:open-pipe 
		       (format NIL "~A~{ \"~A\"~}" 
			interpreter options))
      (condition (err)
	(error "~A~2%~A ~A~2%~A~%~A~{ ~A~}"
	       "Failed to open a stream to the Tcl/Tk interpreter."
	       "Reason: " (princ-to-string err)
	       "The following command was given:"
	       interpreter options))))

(defun CLOSE-TCL/TK-STREAM (STREAM)
  "Instructs the Tcl/Tk process associated with STREAM to exit and
closes the stream. Arguments STREAM must be a stream created by
OPEN-TCL/TK-STREAM. This function is implementation dependent."
  (close stream))

;; ----------------------------------------------------------------------------
;; 2. Communicating with Tcl/Tk
;; ----------------------------------------------------------------------------

(defun SEND-SCRIPT (STREAM SCRIPT)
  "Sends clTcl script SCRIPT to a running Tcl/Tk interpreter via
stream STREAM. The stream must be one that was opened with
OPEN-TCL/TK-STREAM. The script must be a list of strings, each string
being a valid Tcl command."
  (format stream "~{~A~%~}" script)
  (force-output stream))

(defun RECEIVE-LINE (STREAM)
  "Reads a Tcl list from STREAM including the terminating
character. Returns nil if an end of file occurs."
  (prog1 (read-list stream)
    (read-line stream nil nil t)))

;; ----------------------------------------------------------------------------
;; clTcl communication protocol
;;
;; 1. Initialization of clTcl
;; 2. Messages
;; 3. Event loop
;; 4. Tests
;;
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; 1. Initialization of clTcl
;; ----------------------------------------------------------------------------

(defvar *INTERPRETER* "/usr/bin/tclsh"
  "The default Tcl/Tk interpreter. Default value is /usr/bin/tclsh.")

(defvar *STREAM* NIL
  "A two-way-stream connected to a Tcl/Tk interpreter or NIL if no
  such interpreter is running. This stream is used by functions CALL,
  POST and RUN.")

(defvar *TRACE-LEVEL* -1
  "An integer that determines which events are traced in clTcl's
communication protocol between Lisp and Tcl/Tk. Setting a larger
number gives trace messages. Level -1 (the default) is complete
silence, level 0 is errors only, level 1 is errors and events, level 2
is errors, events and event details.")

(defvar *DEBUG* NIL
  "If not nil the commands of a script are send one by one to the
  interpreter. This eases localization of errors. Default nil.")

(defun LOG-STATUS (LEVEL CONTROL-STRING &REST ARGUMENTS)
  "Writes formatted text to *TRACE-OUTPUT* if LEVEL is at least as
high as *TRACE-LEVEL*. Level 0 is for errors, level 1 is for events,
level 2 is for event details."
  (when (<= level *trace-level*)
     (fresh-line *trace-output*)
     (apply #'format *trace-output* control-string arguments)
     (fresh-line *trace-output*)))

(defun HANDSHAKE (STREAM)
  "Sends a message to Tcl/Tk stream to test if it responds. Throws an
error when no appropriate message comes back."
  (log-status 1 "Handshaking...~%-> Sending handshake script")
  (let ((reply (progn
		 (send-script stream
		       #TCL[
		       puts "Tcl/Tk at your service. Version [info tclversion]"
		       flush stdout])
		 (receive-line stream))))
    (if (< 20 (mismatch reply "Tcl/Tk at your service."))
	(log-status 1 "<- Reply: ~A~%Handshake okay" reply)
	(progn 
	  (log-status 0 "Handshake failed, throwing an error.")
	  (error "Handshake with Tcl/Tk failed. Interpreter was ~A"
		 *interpreter*)))))

(defun TCL/TK-SETUP-SCRIPT ()
  "Script that creates Tcl procedures in Tcl/Tk to communicate to Lisp."
  #TCL[

  namespace eval ::cltcl:: {

      namespace export callLisp

      # Counterpart of receive-message on the Lisp side
      proc sendMessage {channel tag message} {
          puts $channel [list $tag $message]
	  flush $channel
      }
  
      # Counterpart of send-message on the Lisp side
      proc receiveMessage {channel} {
          set tag [gets $channel]
	  if {$tag == "exit"} {exit}
	  while { $tag != "script" && $tag != "reply" } {
	      after idle $tag
            set tag [gets $channel]
	      if {$tag == "exit"} {exit}
	  }
	  set message ""
	  set header [gets $channel]
	  while { $header != "DONE" } {
            set line [gets $channel]
	    append message $line\n
	    set header [gets $channel]
	  }
	  return [list $tag $message]
      }
  
      proc callLisp {fun args} {
          if [catch { 
	      sendMessage stdout :event [concat [list $fun] $args]
	      set message [receiveMessage stdin]
	      set tag [lindex $message 0]
	      while {$tag == "script"} {
	          set command [lindex $message 1]
		  if [catch {set tmp [namespace inscope :: $command]} err] {
		      sendMessage stdout :error $err
		  } else {
		      sendMessage stdout :reply $tmp
		  }
		  set message [ receiveMessage stdin]
		  set tag [lindex $message 0]
	      }
	      set result [lindex $message 1]
	  } error] {
	      tk_messageBox -message "Fatal error: $error"
	      exit
	  } else {
	      return $result
	  }
      }
  }])


;; ----------------------------------------------------------------------------
;; 2. Messages
;; ----------------------------------------------------------------------------

(defun SEND-MESSAGE (STREAM TAG MESSAGE)
  "Counterpart is Tcl proc receiveMessage"
  (unless stream
    (error "Trying to use empty clTcl stream."))
  (format stream "~A~%" tag)
  (with-input-from-string (in message)
    (loop for line = (read-line in nil nil)
       while line do
	 (format stream "NEXT~%")
	 (format stream "~A~%" line)))
  (format stream "DONE~%")
  (force-output stream))

(defun RECEIVE-MESSAGE (STREAM)
  "Attempts to read a message from stream. Functions MESSAGE-TAG
yields the tag of the message. Function MESSAGE-DATA yields the
data. Counterpart of Tcl proc sendMessage"
  (unless stream
    (error "Trying to use empty clTcl stream."))
  (let ((message (receive-line stream)))
    (when message
      (let ((list (read-tcl-list-from-string1 message)))
	(when list
	  (list (let ((*read-eval*))
		  (read-from-string (first list)))
		(second list)))))))

(defun MESSAGE-TAG (MESSAGE)
  "The tag of the message obtained with RECEIVE-MESSAGE. One of the
symbols :reply, :event or :error."
  (first message))

(defun MESSAGE-DATA (MESSAGE)
  "The data of the message obtained with RECEIVE-MESSAGE. The contents
depends on the type of message."
  (second message))

;; ----------------------------------------------------------------------------
;; 3. Event loop
;; ----------------------------------------------------------------------------
(defun KEEP-LISTENING ()
  "A restart applicable during errors in events. Transfers control
back to Tcl and resumes listening."
  (invoke-restart 'keep-listening))

(defun HANDLE-EVENT (EVENT)
  "Handles EVENT that was read from a Tcl/Tk stream. Calls the
requested Lisp function and returns the results. Restart
KEEP-LISTENING is available during the event."
  (let ((event-data (read-tcl-list-from-string1 event)))
    (let ((fun (let ((*read-eval*))
		 (read-from-string (first event-data))))
	  (args (rest event-data)))
      (log-status 1 "   Calling event handler ~A" fun)
      (log-status 2 "   with arguments (~{~A~^ ~})" args)
      (let ((result (with-simple-restart 
			(keep-listening
			 (format nil "Abort ~A" fun))
		      (let ((value (apply fun args)))
			(log-status 1 "   Event returned")
			(log-status 2 "   value ~S" value)
			value))))
	(typecase result
	  (list (write-list-to-tcl-string result))
	  (t (princ-to-string result)))))))

(defun LISTEN-FOR-REPLY (STREAM)
  "Receives messages from STREAM until a reply is received. Incoming
events and errors are handled. Returns the data from the reply."
  (loop
     for message = (progn (log-status 2 "Listening for reply...")
			  (receive-message stream))
     for tag = (message-tag message)
     for data = (message-data message)
     do
       (log-status 1 "   <- Incoming ~(~A~)" tag)
       (log-status 2 "      with data ~S" data)
     until (or (null message) (eql tag :reply))
     do (case tag
	 (:event (send-message stream "reply" (handle-event data))
		 (log-status 1 "   -> Reply"))
	 (:error (error data))
	 (t (cerror "Keep listening for reply"
		    "Invalid tag while listening for reply: ~A" tag)))
     finally (return (when message data))))

(defun SCRIPT-FOR-COMMAND (COMMAND ARGUMENTS)
  "Script that performs command with all arguments safely escaped."
  (format-script
   #TCL[~A ~A]
   command
   (write-list-to-tcl-string 
    (mapcar (lambda (x)
	      (typecase x
		(symbol (if (eql (find-package :keyword)
				 (symbol-package x))
			    (format nil "-~(~A~)" x)
			    x))
		(t x)))
	    arguments))))

(defun RUN-SCRIPT (SCRIPT &OPTIONAL (STREAM *STREAM*))
  "Sends a script directly to the Tcl/Tk interpreter and waits for a
reply."
  (send-script stream 
	       (format-script
		#TCL[
		if [catch {
		  if [catch {set reply [~{~A~%~}]} error] {
		    ::cltcl::sendMessage stdout :error $error
		  } else  {
		    ::cltcl::sendMessage stdout :reply $reply
		  }
		} error] {
		  tk_messageBox -message "Fatal error: $error"
		  exit
		}] script))
  (listen-for-reply stream))


(defun RUN-SUB-SCRIPT (SCRIPT &OPTIONAL (STREAM *STREAM*))
  "Sends a script to the listener that triggered the event that
triggered this call and waits for a reply. The script is picked up and
evaluated by a listener at the Tcl/Tk side."
  (send-message stream "script" (format nil "~{~A~%~}" script))
  (listen-for-reply stream))


(defun CALL (COMMAND &REST ARGS)
  "Calls Tcl/Tk command COMMAND with arguments ARGS properly
escaped. Arguments of type symbol are writting to a string and
prefixed with a hyphen to support keyword for Tcl options. Other
arguments are written to string if necessary and properly
escaped. This function is typically used in a event that was invoked
in a clTcl script run by EVENT-LOOP. Sends the command to *STREAM*."
  (log-status 1 "   -> Calling command ~A" command)
  (log-status 2 "      with arguments (~{~A~^ ~})" args)
  (run-sub-script (script-for-command command args) *stream*))

(defun POST (COMMAND &REST ARGS)
  "Posts Tcl/Tk command COMMAND with arguments ARGS properly escaped
as an event and does not wait for a reply. Arguments of type symbol
are writting to a string and prefixed with a hyphen to support keyword
for Tcl options. Other arguments are written to string if necessary
and properly escaped. Sends the command to *STREAM*."
  (log-status 1 "   -> Posting command ~A" command)
  (log-status 2 "      with arguments (~{~A~^ ~})" args)
  (send-script *stream* (script-for-command command args)))

(defun RUN (SCRIPT &REST ARGS)
  "Sends SCRIPT via *STREAM* to Tcl/Tk and waits for a reply. This
function is typically used in a event that was invoked in a clTcl
script run by EVENT-LOOP. Sets Tcl/Tk variable argv to ARGS, properly
escaped."
  (let ((commands (clean-script script)))
    (log-status 1 "   -> Running script of ~A commands" (length commands))
    (log-status 2 "      with arguments (~{~A~^ ~})" args)
    (log-status 3 "~{~A~%~}" commands)
    (run-sub-script (script-for-command "set" (list "argv" args)) *stream*)
    (if *debug*
        (loop
           for line in commands
           for result = (progn
                          (log-status 1 "   -> Running command ~A" line)
                          (run-sub-script (list line) *stream*))
           finally (return result))
        (run-sub-script commands *stream*))))

(defun EVENT-LOOP (SCRIPT &KEY (INTERPRETER *INTERPRETER*) OPTIONS ARGUMENTS)
  "Starts Tcl/Tk interpreter located at INTERPRETER, binds *STREAM* to
the stream that connects to Tcl/Tk, binds *INTERPRETER* to
INTERPRETER, runs SCRIPT on ARGUMENTS and starts listening on the
stream. Use command 'exit' to end the listener. Establishes a binding
for restart KEEP-LISTENING to recover from errors in an event
handler. The default value for INTERPRETER is *INTERPRETER. Keyword
OPTIONS is passed to the interpreter."
  (let ((*interpreter* interpreter))
    (with-tcl/tk (*stream* :interpreter interpreter 
			   :options options)
      (loop
	 initially 
	   (handshake *stream*)
	   (log-status 1 "Setting up Tcl/Tk side...")
	   (run-script (Tcl/Tk-setup-script) *stream*)
	   (log-status 1 "Tcl/Tk side ready")
	   (log-status 1 "Running initial script")
	   (log-status 3 "~{~A~%~}" script)
	   (send-script *stream* 
			(script-for-command "set" (list "argv" arguments)))
	   (if *debug*
	       (dolist (x (clean-script script))
		 (log-status 1 "   -> Running command ~A" x)
		 (run-script (list x) *stream*))
	       (run-script script *stream*))
	   (log-status 1 "Initial script done")
	 for message = (progn (log-status 2 "Event loop listening...")
			      (receive-message *stream*))
	 do 
	   (log-status 1 "<- Incoming ~(~A~)" (message-tag message))
	   (log-status 2 "   with data ~S" (message-data message))
	 while message do
	   (if (eql (message-tag message) :event)
	       (progn (send-message *stream* "reply"
				    (handle-event (message-data message)))
		      (log-status 1 "-> Reply"))
	       (cerror "Keep listening for events."
		       "Invalid clTcl message: ~A" message))
	 finally
	   (log-status 1 "Event loop stopped")))))

;; ----------------------------------------------------------------------------
;; 4. Tests
;; ----------------------------------------------------------------------------

(defun TEST (&REST ARGS &KEY (TRACE-LEVEL 0) &ALLOW-OTHER-KEYS)
  "Tests the connection with Tcl/Tk and displays systems
information. Sets *TRACE-LEVEL* to TRACE-LEVEL. The other keywords are
passed to the event handler. See OPEN-TCL/TK-STREAM for allowed
keywords."
  (let ((script #TCL[

	  package require Tk
	  namespace import ::cltcl::callLisp
	  wm protocol . WM_DELETE_WINDOW exit

	  proc displayinfo {} {

	      lappend info \
	          "Machine:" "[callLisp machine-type] \
                             ([callLisp machine-version])" \
	          "System:" "[callLisp software-type]\
                            ([callLisp software-version])" \
	          "Common Lisp:" "[callLisp lisp-implementation-type]\
                                 ([callLisp lisp-implementation-version])" \
	          "Windowing:" "[tk windowingsystem]" \
 	          "Tcl:" "[info tclversion] ([info patchlevel])" \
	          "Executable:" "[info nameofexecutable]" \
	          "Library:" "[callLisp cltcl::funA]" \
		  "Encoding:" "[encoding system]"

	      set i 0
	      foreach {x y} $info {
	          label .tag$i -text $x
		  message .info$i -text $y -width 500
		  grid .tag$i .info$i -sticky nw
		  incr i
	      }

	      button .exitButton \
	          -width 10 \
		  -text "Close" \
	          -command {exit}
	      grid .exitButton -columnspan 2 -pady 4
	  }

	  proc funB {fun} {
	    return [callLisp $fun]
	  }

	  displayinfo

	  ])
	(*trace-level* trace-level))
    (apply #'cltcl:event-loop script :allow-other-keys t args)))

(defun funA ()
  "Helper for TEST"
  (cltcl:call "funB" "cltcl::funC"))

(defun funC ()
  "Helper for TEST"
  (cltcl:call "info" "library"))


(defun RUN-REPL (&REST ARGS &KEY (TRACE-LEVEL 0) &ALLOW-OTHER-KEYS)
  "Stub to test function REPL. Demonstrates how the repl might be
added to an application via an event-handler."
  (let ((*trace-level* trace-level))
    (apply #'cltcl:event-loop #TCL[	  package require Tk
	  namespace import ::cltcl::callLisp
	  wm protocol . WM_DELETE_WINDOW {tk_messageBox -message "Type 'exit' in the repl"}
	  callLisp cltcl::repl]
	  :allow-other-keys t args)))

(defun REPL ()
  "A read-eval-print loop. Useful for development. Prompts for
commands to send to the Tcl/Tk interpreter and prints the reply."
  (loop 
     initially 
       (call "update" "idletasks")
       (fresh-line)
       (format t "Welcome in the clTcl repl~%Type 'exit' to quit.~%")
     for command = (progn (fresh-line)
			  (princ "% ")
			  (receive-line *standard-input*))
     until (or (equal command "exit") 
	       (null command)
	       (null (handler-case 
			 (loop
			    initially (send-message *stream* "script" command)
			    for line = (receive-line *stream*)
			    for items = (when line
					  (read-tcl-list-from-string1 line))
			    for possible-tag = (first items)
			    until (or (null line)
				      (equal possible-tag ":reply")
				      (equal possible-tag ":error"))
			    do (princ line)
			    finally 
			      (when line (call "update" "idletasks"))
			      (return (princ (second items))))
		       (condition (error)
			 (princ error)))))
     finally 
       (when (equal command "exit")
	 (send-message *stream* "script" "exit"))
       (princ "bye")
       (fresh-line)))

