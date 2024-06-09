;;;;
;;;;process graph
;;;;
(defvar *main-status* "<wait>")
(defvar *md-date-only* 1)         ;1 = "mm/dd/yyyy" 
(defvar *md-date-time* 2)         ;2 = "mm/dd/yyyy" hh:nn"
(defvar *graph-name* nil)
(defvar *seed* nil)
(defvar *cluster-name* nil)
(defvar *flag-seed-doc* nil)      ;document
(defvar *tmp-obj-doc* nil)
(defvar *flag-line* 40)
(defvar *flag-stats* 22)
(defvar *node* nil)
(defvar *cnn* nil)
(defvar *pstack* nil)
(defvar *hop* nil)
(defvar *flow* nil)
(defvar *action-flags* nil)
(defvar *conversation-proc* nil) ;could evolve to a key if multiple conv
(defvar *learning-mode* 0)
(defvar *debug-mode* 0)
(defvar *voice-repeat* 0)
(defvar *voice-dialog* 0)
(defvar *pct-answer* 70)
(defvar *myplace* "Pully")
(defvar *myname* "Atalia")
(defvar *repl-msg* nil)
(defvar *dialog* nil)
(defvar *last-answer* nil)
(defvar *child-answer* nil)
(defvar *entry-node* nil)
(defvar *dicos-path* "/home/arkho/.emacs.d/dicos/")
(defvar *dico-10k* "eng-10k-nx.txt")
(defvar *dico-oxf* "eng-oxf.txt")
(defvar *dico-eng* "eng-alpha.txt")
(defvar *pnum* "0123456789")
(defvar *dec-sign* ".")
(defvar *palpha* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defvar *pend* ".?!")
(defvar *psigns* (concatenate 'string ".:;,?!-_¬¢^£$¨%{}[]~()&*'§+=@|/><°" '(#\") '(#\\)))
(defvar *path-delim* "/")
(defvar *root-path* "/home/arkho/.emacs.d/")
(defvar *tmp-path* "/home/arkho/.emacs.d/tmp/")
(defvar *c-path* "/home/arkho/.emacs.d/C/")
(defvar *c++-path* "/home/arkho/.emacs.d/C++/")
(defvar *sh-path* "/home/arkho/.emacs.d/sh/")
(defvar *r-path* "/home/arkho/.emacs.d/R/")
(defvar *rdf-path* "/home/arkho/.emacs.d/rdf/")
(defvar *gnu-scripts-path* "/home/arkho/.emacs.d/gnuplot/")
(defvar *plot-tmp-2d1f* "plot-tmp-2d1f")
(defvar *rshebang* "/usr/bin/Rscript")
(defvar *gnuplot* "/usr/local/bin/gnuplot")
(defvar *dot* "/usr/bin/dot")
(defvar *gviz-scripts* "/home/arkho/.emacs.d/graphviz/")
(defvar *gedit* "/usr/bin/gedit")     ;text editor
(defvar *evince* "/usr/bin/evince")   ;document viewer
(defvar *pdftotext* "/usr/bin/pdftotext")   ;pdf to text converter
(defvar *html2text* "/usr/bin/html2text")
(defvar *curl* "/usr/bin/curl")
(defvar *soffice* "/usr/bin/soffice") 
(defvar *dragonfire* "/usr/bin/dragonfire")
(defvar *python* "/usr/bin/python3")
(defvar *bash* "/bin/bash")
(defvar *flite* "/usr/bin/flite")
(defvar *gnome-terminal* "/usr/bin/gnome-terminal")
(defvar *avatar-pg* "avatar-cp4")
(defvar *avatar-cfg* "avatar-cp4.cfg")
(defvar *avatar-srv* "srv-avatar2")
(defvar *showtrs* "showtrs")
(defvar *avatar-gtk2* "avatar")
(defvar *ps-t01* nil)
(defvar *nd-ref* (make-array 8192 :fill-pointer 0 :adjustable t))
(define-alien-variable ("dynamic_space_size" dynamic-space-size-bytes) unsigned-long)


(defun heap-n-bytes ()
  (+ dynamic-space-size-bytes
     (- sb-vm::read-only-space-end sb-vm::read-only-space-start)
     (- sb-vm::static-space-end sb-vm::static-space-start)))

;;;; string misc
;					
;  (string-trim '(#\space) "   128.56   ")    ===> "128.56"
;  (setf n1 (read-from-string  (string-trim '(#\space) "   128.56   "))) ===> 128.56 single-float att no string nil
;
(defun tl-split (chars str &optional (lst nil) (accm ""))   ;(tl-split '(#\space) "Hello world")  (tl-split '(#\,) "John,is going,mad  , !")
  (cond
    ((= (length str) 0) (reverse (cons accm lst)))
    (t
     (let ((c (char str 0)))
       (if (member c chars)
   (tl-split chars (subseq str 1) (cons accm lst) "")
   (tl-split chars (subseq str 1) 
                        lst 
                        (concatenate 'string accm (string c))))
            ))))

(defun tl-join (str lst &optional (jstr ""))   ;(tl-join "-" '("this" "is" "going" "to" "be" "a" "single" "string"))
  (cond
    ((null lst) jstr)
    (t (let ((news (concatenate 'string
    jstr
    (first lst)
    (if (null (rest lst))
       ""
       str))))
	 (tl-join str (rest lst) news)))))

(defun replace-all (string part replacement &key (test #'char=))
    ;"Returns a new string in which all the occurences of the part is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
         while pos)))

(defun make-uppercase (s)
   (return-from make-uppercase (format nil "~@:(~a~)" s))
  )

(defun make-lowercase (s)
   (return-from make-lowercase (format nil "~(~a~)" s))
   )

(defun enum-string-list (ls)
 (let ((s nil))
  (if (equal (typep ls 'cons) T)
      (progn
        (dolist (z (reverse ls))
          (setf s (concatenate 'string "§" z "§ " s)))
	(return-from enum-string-list s)
	))
  ))

(defun get-file-content (fs)
  (let ((content nil)(i 0))
   (with-open-file (stream fs
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
      (when stream
      (loop for line = (read-line stream nil)
       while line do
         (setf content (concatenate 'string content  line))
	 (setf i (+ i 1)))
      (close stream)
      (return-from get-file-content content)))))

(defun remove-duplicate-from-list (ls)
  (let ((new-ls nil)(b1 nil))
    (if (equal ls nil) (return-from remove-duplicate-from-list nil))
    (dolist (z ls)
      (if (equal new-ls nil)(push z new-ls)
	  (progn
	    (setf b1 T)
            (dolist (w new-ls)
               (if (equal w z)(setf b1 nil))
	       )
	    (if (equal b1 T)(push z new-ls))
	    ))     
      )
    (return-from remove-duplicate-from-list (reverse new-ls))
    )
  )

(defun remove-meaningless-from-list (ls)       ;filter garbage
  (let ((new-ls nil)(b nil))
    (if (equal ls nil) (return-from remove-meaningless-from-list nil))
    (dolist (z ls)	 
       (setf b (ppcre:all-matches-as-strings "[a-zA-Z]+" z))
       (if (not (equal b nil))(push z new-ls))  
      )
    (return-from remove-meaningless-from-list (reverse new-ls))
    )
  )

(defun is-numeric (s c)     ;c = decimal sep such as "."
  (let ((r  nil)(j 0)(s1 nil)(c1 nil)(k 0)(b1 T))
    (setf s1 (string-trim " " s))
    (loop
       (when (>= j (length s1)) (return))
       (setf c1 (subseq s1 j (+ j 1)))
       (if (not (equal (search c1 *pnum*) nil))(setf k (+ k 1)))
       (if (and (equal b1 T) (equal c1 c))
	   (progn
	     (setf k (+ k 1))
	     (setf b1 nil)))	   
       (setf j (+ j 1))
       )
  (if (equal k (length s1)) (setf r T)) 
  (return-from is-numeric r)
  ))
;;;;end string
;;;;
;;;; Misc
;;;;

(defun leap-year (year)
  (or (zerop (rem year 400))
      (and (zerop (rem year 4))
           (not (zerop (rem year 100)))
      )))

(defun current-datetime-string ()
  (multiple-value-bind 
      (s mn h d mt y dow dst-p tz)
      (get-decoded-time)
      (declare (ignore dst-p))
      ;(return-from current-datetime-string (format nil "~2,'0d:~2,'0d:~2,'0d of ~a, ~d-~2,'0d-~d (GMT~@d)"
      (return-from current-datetime-string (format nil "~2,'0d:~2,'0d:~2,'0d of ~a, ~4,'0d-~2,'0d-~d (GMT~@d)"
	h
	mn
	s
	(nth dow '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
	y
	mt
	d
	(- tz)))))

(defun get-time ()
  (let ((gt nil))
    (setf gt (concatenate 'string  (format nil "~2,'0d" (nth-value 2 (get-decoded-time))) ":"
			 (format nil  "~2,'0d" (nth-value 1 (get-decoded-time)))))
    (return-from get-time gt)
    )
  )

(defun day-of-week (day month year)
  (nth-value 6 (decode-universal-time (encode-universal-time 0 0 0 day month year 0))))

(defun day-name (n)
  (let ((w '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")))
	(if (< n 7) (return-from day-name (elt w n))
	    (return-from day-name nil))))

(defun make-universal-time (dt f)
  (let ((ls nil) (tu nil))
  (if (equal f  1)                 ; format 1 mm/dd/yyyy
      (progn
	(setf ls (tl-split '(#\/) dt))
	(setf tu (encode-universal-time 0 0 0 (parse-integer(elt ls 1)) (parse-integer(elt ls 0)) (parse-integer(elt ls 2))))
	(return-from make-universal-time tu)
	))
  ))

(defun get-day (dt f)
  (let ((ls nil) (tu nil))
  (if (equal f  1)                 ; format 1 mm/dd/yyyy
      (progn
	(setf ls (tl-split '(#\/) dt))
	(setf tu (parse-integer(elt ls 1)))
	(return-from get-day tu)
	))
  ))

(defun get-month (dt f)
  (let ((ls nil) (tu nil))
  (if (equal f  1)                 ; format 1 mm/dd/yyyy
      (progn
	(setf ls (tl-split '(#\/) dt))
	(setf tu (parse-integer(elt ls 0)))
	(return-from get-month tu)
	))
  ))

(defun get-year (dt f)
  (let ((ls nil) (tu nil))
  (if (equal f  1)                 ; format 1 mm/dd/yyyy
      (progn
	(setf ls (tl-split '(#\/) dt))
	(setf tu (parse-integer(elt ls 2)))
	(return-from get-year tu)
	))
  ))

(defun get-mfirst (dt f)
  (let ((ls nil) (tu nil))
  (if (equal f  1)                 ; format 1 mm/dd/yyyy
      (progn
	(setf ls (tl-split '(#\/) dt))
	(setf tu (concatenate 'string (elt ls 0) "/01/" (elt ls 2))) 
	(return-from get-mfirst tu)
	))
  ))

(defun get-mlast (dt f)
  (let ((ls nil) (tu nil) (mt (make-array 13 :initial-contents '(0 31 28 31 30 31 30 31 31 30 31 30 31))))
  (if (equal (leap-year (get-year dt f)) t) (setf (aref mt 2) 29))
  (if (equal f  1)                 ; format 1 mm/dd/yyyy
      (progn
	(setf ls (tl-split '(#\/) dt))
	(setf tu (concatenate 'string (elt ls 0) "/" (format nil "~a" (aref mt (get-month dt f))) "/" (elt ls 2))) 
	(return-from get-mlast tu)
	))
  ))
;;;;
;;;;end misc
;;;;

(defun set-learning-lvl (lvl)
  (setf *learning-mode* 0)                   ;0 not learning
  (if (= lvl 1) (setf *learning-mode* 1))    ;1 learning, supervised, all
  (if (= lvl 2) (setf *learning-mode* 2))    ;2 learning, supervised, interested
  (if (= lvl 3) (setf *learning-mode* 3))    ;3 learning, unsupervised, all 
  (if (= lvl 4) (setf *learning-mode* 4))    ;4 learning, unsupervised, interested
  (return-from set-learning-lvl T)
  )

(defun get-learning-auth ()
  (let ((b1 nil))
    (if (= *learning-mode* 0) (setf b1 nil))
    (if (= *learning-mode* 1) (setf b1 T))
    (if (= *learning-mode* 2) (setf b1 T))
    (if (= *learning-mode* 3) (setf b1 T))
    (if (= *learning-mode* 4) (setf b1 T))
    (return-from get-learning-auth b1)
    ))

(defun set-voice-active (repeat  dialog)
  (setf *voice-repeat* 0)            ; repeat 1  = say input text
  (setf *voice-dialog* 0)            ; dialog 1  = say the answer
  (if (= repeat 1) (setf *voice-repeat* 1))
  (if (= dialog 1) (setf *voice-dialog* 1))
  )

(defun set-debug (debug)
  (setf *debug-mode* 0)
  (if (= debug 1) (setf *debug-mode* 1))
  )

(defun create-seed-fast (name xdata weight bias)
   (let ((xs (list :name name :desc (concatenate 'string (current-datetime-string) " | ")
		   :xdata xdata :weight weight :bias bias)))
     (push xs *seed*)
     (return-from create-seed-fast t)
	)
   )

(defun create-seed-sense (name xdata weight bias)
   (let ((xs (list :name name :desc (concatenate 'string (current-datetime-string) " | ")
		   :xdata xdata :weight weight :bias bias))
	 (b1 t))
        (if (equal (length *seed*) 0)
            (progn
            (push xs *seed*)
            (return-from create-seed-sense t))
            (progn
               (dolist (vs *seed*)
                   (when (equal (format nil "~a" (getf vs :name)) (format nil "~a" name))
                     (setq b1 nil)
                     (return)))
               (if (equal b1 t)
                   (progn
                   (push xs *seed*)
                   (return-from create-seed-sense t))
                   (return-from create-seed-sense nil))))))      

(defun create-seed (name xdata weight bias)
   (let ((xs (list :name name :desc (concatenate 'string (current-datetime-string) " | ")
		   :xdata xdata :weight weight :bias bias))
	 (b1 t))
        (if (equal (length *seed*) 0)
            (progn
            (push xs *seed*)
            (return-from create-seed t))
            (progn
               (dolist (vs *seed*)
                   (when (equal (format nil "~(~a~)" (getf vs :name)) (format nil "~(~a~)" name))
                     (setq b1 nil)
                     (return)))
               (if (equal b1 t)
                   (progn
                   (push xs *seed*)
                   (return-from create-seed t))
                   (return-from create-seed nil))))))        

(defun mod-seed (name xdata weight bias)
    (dolist (sd *seed*)
      (when (equal (getf sd :name) name)
        (let ((s1 sd))
	  (setf (getf s1 :xdata) xdata)
	  (setf (getf s1 :weight) weight)
	  (setf (getf s1 :bias) bias)
           (setq *seed* (substitute s1 sd *seed*)))
	  (return-from mod-seed t))))

(defun list-all-seed ()
  (dolist (sd *seed*)
    (format t "~{~a:~10t~a~%~}~%" sd)))

(defun find-seed (name)
  (dolist (sd *seed*)
     (when (equal (format nil "~(~a~)" (getf sd :name)) (format nil "~(~a~)" name))
        (format t "~{~a:~10t~a~%~}~%" sd)
        (return-from find-seed t))))

(defun find-seed2 (name)
  (dolist (sd *seed*)
      (when (equal (format nil "~(~a~)" (getf sd :name)) (format nil "~(~a~)" name))
        (return-from find-seed2 sd))))

(defun add-line ()
  (let ((s1 nil)(s2 nil))
    (setf s1 (with-output-to-string (stream)
    (loop
       (setf s2 (read-line))
       (if (= (length s2) 0)
	   (return)
	   (write-line s2 stream))
       )
    ))
   (return-from add-line s1)))

(defun update3-desc-seed (name desc)
  (dolist (sd *seed*)
     (when (equal (getf sd :name) name)
        (let ((s1 (getf sd :desc)) (s3 nil)(s4 nil))
           (when (not (eq (position #\| s1) nil))
                 (setq s4 (subseq s1 0 (+ (position #\| s1) 1))))
           (setf s3 (substitute (concatenate 'string s4 " " (string-trim "\"" desc)) (getf sd :desc)  sd))
           (setq *seed* (substitute s3 sd *seed*)))
        (return-from update3-desc-seed t))))

(defun add-to-desc-seed (sdname txt)
  (let ((sd nil)(s2 nil))
    (dotimes (i (length *seed*))
      (when (equal (getf (elt *seed* i) :name) sdname)
	 (setf s2 (concatenate 'string (getf (elt *seed* i) :desc) " " txt))
         (setf sd (substitute s2 (getf (elt *seed* i) :desc)  (elt *seed* i)))
	 (setq *seed* (substitute sd (elt *seed* i) *seed*))
	 (setf sd (elt *seed* i))
	 (return-from add-to-desc-seed (cons i sd))
	)
      )

    (return-from add-to-desc-seed nil)
    )
  )

(defun update2-desc-seed (name desc)
  (dolist (sd *seed*)
     (when (equal (getf sd :name) name)
        (let ((s1 nil))
           (setf s1 (substitute desc (getf sd :desc)  sd))
           (setq *seed* (substitute s1 sd *seed*)))  
        (return-from update2-desc-seed t))))

(defun update-desc-seed (name)
  (dolist (sd *seed*)
     (when (equal (getf sd :name) name)
        (format t "~{~a:~10t~a~%~}~%" sd)
        (format t "Enter description : ")
        (let ((s1 (getf sd :desc)) (s2 nil) (s3 nil)(s4 nil))
           (setq s2 (with-output-to-string (s)
           (format s "~s" (add-line))))
           (when (not (eq (position #\| s1) nil))
                 (setq s4 (subseq s1 0 (+ (position #\| s1) 1))))
           (setf s3 (substitute (concatenate 'string s4 " " (string-trim "\"" s2)) (getf sd :desc)  sd))
	   ;(setf s3 (substitute (concatenate 'string s4 " " s2) (getf sd :desc)  sd))
           (setq *seed* (substitute s3 sd *seed*))
           (terpri)
           (find-seed name))  
        (return-from update-desc-seed t))))

(defun get-desc-from-seed (name)
  (let ((sd (find-seed2 name)) (s1 nil))
    (if (equal sd nil)
	(return-from get-desc-from-seed nil)
	(progn
          (setf s1 (getf sd :desc))
          (return-from get-desc-from-seed s1)))))

(defun create-node-sense (name)
   (let ((xn (list :name name :set-seed (make-array 3 :fill-pointer 0 :adjustable t)
		   :set-next (make-array 3 :fill-pointer 0 :adjustable t) :mathfun "passthru" :bias 0))
	 (b1 t))
        (if (equal (length *node*) 0)
            (progn
            (push xn *node*)
            (return-from create-node-sense t))
            (progn
               (dolist (nd *node*)
                  (when (equal (format nil "~a" (getf nd :name)) (format nil "~a" name))
                     (setq b1 nil)
                     (return)))
               (if (equal b1 t)
                   (progn
                   (push xn *node*)
                   (return-from create-node-sense t))
                   (return-from create-node-sense nil))))))

(defun create-node (name)
   (let ((xn (list :name name :set-seed (make-array 3 :fill-pointer 0 :adjustable t)
		   :set-next (make-array 3 :fill-pointer 0 :adjustable t) :mathfun "passthru" :bias 0))
	 (b1 t))
        (if (equal (length *node*) 0)
            (progn
            (push xn *node*)
            (return-from create-node t))
            (progn
               (dolist (nd *node*)
                  (when (equal (format nil "~(~a~)" (getf nd :name)) (format nil "~(~a~)" name))
                     (setq b1 nil)
                     (return)))
               (if (equal b1 t)
                   (progn
                   (push xn *node*)
                   (return-from create-node t))
                   (return-from create-node nil))))))

(defun create-node2-fast (name mathfun bias )
   (let ((xn (list :name name :set-seed (make-array 3 :fill-pointer 0 :adjustable t)
		   :set-next (make-array 3 :fill-pointer 0 :adjustable t) :mathfun mathfun :bias bias)))
     (push xn *node*)
     (return-from create-node2-fast t)
	)
   )

(defun create-node2-sense (name mathfun bias )
   (let ((xn (list :name name :set-seed (make-array 3 :fill-pointer 0 :adjustable t)
		   :set-next (make-array 3 :fill-pointer 0 :adjustable t) :mathfun mathfun :bias bias))
	 (b1 t))
        (if (equal (length *node*) 0)
            (progn
            (push xn *node*)
            (return-from create-node2-sense t))
            (progn
               (dolist (nd *node*)
                  (when (equal (format nil "~a" (getf nd :name)) (format nil "~a" name))
                     (setq b1 nil)
                     (return)))
               (if (equal b1 t)
                   (progn
                   (push xn *node*)
                   (return-from create-node2-sense t))
                   (return-from create-node2-sense nil))))))


(defun create-node2 (name mathfun bias )
   (let ((xn (list :name name :set-seed (make-array 3 :fill-pointer 0 :adjustable t)
		   :set-next (make-array 3 :fill-pointer 0 :adjustable t) :mathfun mathfun :bias bias))
	 (b1 t))
        (if (equal (length *node*) 0)
            (progn
            (push xn *node*)
            (return-from create-node2 t))
            (progn
               (dolist (nd *node*)
                  (when (equal (format nil "~(~a~)" (getf nd :name)) (format nil "~(~a~)" name))
                     (setq b1 nil)
                     (return)))
               (if (equal b1 t)
                   (progn
                   (push xn *node*)
                   (return-from create-node2 t))
                   (return-from create-node2 nil))))))

(defun add-seed-to-node (seed-name node-name)
   (dolist (nd *node*)
      (when (equal (getf nd :name) node-name)
         (dolist (sd *seed*)
            (when (equal (getf sd :name) seed-name)
                (vector-push-extend seed-name (getf nd :set-seed))
                ;(print (type-of (getf nd :set-seed)))
                (return-from add-seed-to-node t))))))

(defun add-next-to-node-fast (node-name-next node-name-sel)
       (dolist (nd1 *node*)
          (when (equal (getf nd1 :name) node-name-sel)
                (vector-push-extend node-name-next (getf nd1 :set-next))
                (return-from add-next-to-node-fast t)))
   )

(defun add-next-to-node (node-name-next node-name-sel)
   (let ((b1 nil))
      (dolist (nd2 *node*)
         (when (equal (getf nd2 :name) node-name-next)
               (setq b1 t)
               (return)))
     (if (equal b1 t)
         (dolist (nd1 *node*)
            (when (equal (getf nd1 :name) node-name-sel)
                (vector-push-extend node-name-next (getf nd1 :set-next))
                (return-from add-next-to-node t))))))

(defun mk-cadac-node (node-name)
  (let ((nm nil))
    (setf nm (replace-all node-name '(#\space) "-"))
    (setf nm (concatenate 'string nm "-cadac"))
    (if (not (equal (create-seed nm 1 1 -1) nil))
	(progn
	  (add-seed-to-node nm node-name)
	  (return-from mk-cadac-node nm))
	(return-from mk-cadac-node nil))
    ))

(defun mod-mathfun-node (node-name mathfun)
     (dolist (nd *node*)
      (when (equal (getf nd :name) node-name)
          (let ((n1 nil))
           (setf n1 (substitute mathfun (getf nd :mathfun)  nd))
           (setq *node* (substitute n1 nd *node*)))
	  (return-from mod-mathfun-node t))))

(defun mod-bias-node (node-name bias)
     (dolist (nd *node*)
      (when (equal (getf nd :name) node-name)
          (let ((n1 nil))
           (setf n1 (substitute bias (getf nd :bias)  nd))
           (setq *node* (substitute n1 nd *node*)))
	  (return-from mod-bias-node t))))

(defun mod-bias-node2 (i-node bias)  ;i-node INTEGER
  (let ((n1 nil) (nd nil))
    (setf nd (elt *node* i-node))
    (setf n1 (substitute bias (getf nd :bias)  nd))
    (setq *node* (substitute n1 nd *node*))   
    ))

(defun delete-node (name)
  (dolist (nd *node*)
     (when (equal (getf nd :name) name)
        (setq *node* (delete nd *node*))   
        (return-from delete-node t))))

(defun remove-node (name)
  (dolist (nd *node*)
     (when (equal (getf nd :name) name)
        (setq *node* (remove nd *node*))   
        (return-from remove-node t))))

(defun list-all-node ()
  (dolist (nd *node*)
    (format t "~{~a:~10t~a~%~}~%" nd)))

;;example :  (list-node-per-seed nil '("question" "best-answer" "round-robin" "order") "and" 1)
;;example all index of nodes (list-node-per-seed nil 'nil "not" 1)

(defun list-node-per-seed (lnode lseed cnd-op &optional (p 0))     ;cnd-op "and" "or" "not" lnode list of INDEX nodes lseed list of seed names
  (let ((lres nil)(nd nil)(k 0)(ssx nil))
    (if (equal lnode nil)
	(progn
	  (dotimes (i (length *node*))
	    (push i lnode))
	  (setf lnode (reverse lnode))
	  ))
    ;(print lnode)
    (dolist (ls lseed)
      (setf ssx (concatenate 'string ssx ls)))
    ;(print ssx)
    (dolist (i1 lnode)
      (setf nd (elt *node* i1))
      (setf k 0)
      (dotimes (i (length (getf nd :set-seed)))
	(if (not (equal (search (elt (getf nd :set-seed) i) ssx) nil))
	    (setf k (+ k 1))))
      ;(print k)
      (if (and (equal cnd-op "and") (equal k (length lseed)))
	  (push i1 lres))
      (if (and (equal cnd-op "or") (> k 0) (<= k (length lseed)))
	  (push i1 lres))
      (if (and (equal cnd-op "not") (equal k 0))
	  (push i1 lres))
      )
    (if (> (length lres) 0) (setf lres (reverse lres)))
    (if (= p 1)
	(progn
          (dolist (i2 lres)
	    (format t "~{~a:~10t~a~%~}~%" (elt *node* i2)))))
    (return-from list-node-per-seed lres)
   ))

(defun find-node (name)
    (dolist (nd *node*)
      (when (equal (format nil "~(~a~)" (getf nd :name)) (format nil "~(~a~)" name))
        (format t "~{~a:~10t~a~%~}~%" nd)
        (return-from find-node t)))
  )

(defun find-node-sense (name)
  (dolist (nd *node*)
    ;(if (not (equal (search "36" (format nil "~a" (getf nd :name))) nil))(print (getf nd :name))) 
     (when (equal (format nil "~a" (getf nd :name)) (format nil "~a" name))
        (format t "~{~a:~10t~a~%~}~%" nd)
        (return-from find-node-sense t))))

(defun find-node2 (name)
  (dolist (nd *node*)
      (when (equal (format nil "~(~a~)" (getf nd :name)) (format nil "~(~a~)" name))
        (return-from find-node2 nd))))

(defun find-node2-sense (name)
  (dolist (nd *node*)
      (when (equal (format nil "~a" (getf nd :name)) (format nil "~a" name))
        (return-from find-node2-sense nd))))

(defun clone-node (sel-name new-name)
  (let ((nd nil) (cdc nil)(s1 nil))
    (setf nd (find-node2 sel-name))
    (if (equal nd nil) (return-from clone-node nil))
    ;(setf k (position nd  *node*))
    (if (not (equal (create-node2 new-name (getf nd :mathfun) 0) nil))
	(progn
	  (dotimes (i (length (getf nd :set-seed)))
	    (if (not (equal (search "-cadac" (elt (getf nd :set-seed) i)) nil))
		     (progn
		       (setf cdc (mk-cadac-node new-name))
		       (setf s1 (get-desc-from-seed (elt (getf nd :set-seed) i)))
		       (update2-desc-seed cdc s1))
		      (add-seed-to-node (elt (getf nd :set-seed) i) new-name)))
	  (dotimes (i (length (getf nd :set-next)))
	    (if (equal (elt (getf nd :set-next) i) (getf nd :name))
		(add-next-to-node new-name new-name)
	        (add-next-to-node (elt (getf nd :set-next) i) new-name)))
	  (return-from clone-node T))
	(return-from clone-node nil))
    ))

(defun get-parent-node (i-node) 
  ;return a list / i-node index of node
  (let ((ls nil)(nm nil)(nd nil))
    (setf nm (getf (elt *node* i-node) :name))	
    (dotimes (i (length *node*))
      (setf nd (elt *node* i))
      (dotimes (j (length (getf nd :set-next)))
        (if (equal (elt (getf nd :set-next) j) nm)
	    (progn
            (push i ls)
	    (return)))
	)
      )
    (return-from get-parent-node ls)
    ))

(defun get-child-node (i-node)
  ;return a list / i-node index of node
  (let ((ls nil)(nd nil))
    (setf nd (elt *node* i-node))
    (dotimes (i (length (getf nd :set-next)))
      (dotimes (j (length *node*))
        (if (equal (elt (getf nd :set-next) i) (getf (elt *node* j) :name))
	    (progn
	      (push j ls)
	      (return)))
	)
      )
    (return-from get-child-node ls)
    ))

(defun is-loop (nd)  ;nd node object
  (let ((b1 nil))
    (dotimes (i (length (getf nd :set-next)))
      (if (equal (elt (getf nd :set-next) i) (getf nd :name))
          (progn
	    (setf b1 T)
	    (return))))
    (return-from is-loop b1)
    ))

(defun is-cadac (nd)  ;nd node object
  (let ((b1 nil))
    (dotimes (i (length (getf nd :set-seed)))
      (if (not (equal (search "-cadac" (elt (getf nd :set-seed) i)) nil))
          (progn
	    (setf b1 T)
	    (return))))
    (return-from is-cadac b1)
    ))

(defun is-cadac2 (nd)  ;nd node object
  (let ((sd-name nil))
    (dotimes (i (length (getf nd :set-seed)))
      (if (not (equal (search "-cadac" (elt (getf nd :set-seed) i)) nil))
          (progn
	    ;(setf b1 T)
	    (setf sd-name (elt (getf nd :set-seed) i))
	    (return))))
    (return-from is-cadac2 sd-name)
    ))

(defun remove-cadac (name) ;seed name cadac is specific to each node)
  (if (equal (search "-cadac" name) nil) (return-from remove-cadac nil))
  (dolist (sd *seed*)
     (when (equal (getf sd :name) name)
        (setq *seed* (remove sd *seed*))  
        (return-from remove-cadac t))))

(defun is-recurrent (nd)  ;nd node object
  (let ((b1 nil))
    (dotimes (i (length (getf nd :set-seed)))
      (if (not (equal (search "recurrent" (elt (getf nd :set-seed) i)) nil))
          (progn
	    (setf b1 T)
	    (return))))
    (return-from is-recurrent b1)
    ))

(defun rm-seed-from-node (seed-name node-name)
   (dolist (nd *node*)
      (when (equal (getf nd :name) node-name)
         (let ((s1 (make-array 3 :fill-pointer 0 :adjustable t))(n1 nil)(b1 nil))
              (dotimes (i (length (getf nd :set-seed)))
                 ;(print i)
                 (if (and (equal (length (getf nd :set-seed)) 1) (equal (elt (getf nd :set-seed) 0) seed-name))
                     (progn
                     (setq b1 t)
                     (return)))
                 (if (equal (elt (getf nd :set-seed) i) seed-name)
                     (setq b1 t)
                     (progn
                     (vector-push-extend (elt (getf nd :set-seed) i) s1))))  
              (if (equal b1 nil)
                  (return-from rm-seed-from-node nil)
                  (progn
                  (setq n1 (substitute s1 (getf nd :set-seed) nd))
                  (setq  *node* (substitute n1 nd *node*))
                  (return-from rm-seed-from-node t)))))))

(defun rm-seed-from-all-node (seed-name)
  (let ((i1 0))
   (dolist (nd *node*)
      (let ((s1 (make-array 3 :fill-pointer 0 :adjustable t))(n1 nil)(b1 nil))
           (dotimes (i (length (getf nd :set-seed)))
              (if (and (equal (length (getf nd :set-seed)) 1) (equal (elt (getf nd :set-seed) 0) seed-name))
                  (progn
                  (setq b1 t)
                  (return)))
              (if (equal (elt (getf nd :set-seed) i) seed-name)
                  (setq b1 t)
                  (progn
                  (vector-push-extend (elt (getf nd :set-seed) i) s1))))  
           (if (equal b1 nil)
               (setf i1 (+ i1 0))
               (progn
               (setq n1 (substitute s1 (getf nd :set-seed) nd))
               (setq  *node* (substitute n1 nd *node*))
               (setf i1 (+ i1 1))))))
  (if (> i1 0)
      (return-from rm-seed-from-all-node t)
      (return-from rm-seed-from-all-node nil))))

(defun rm-next-from-node (node-name-next node-name-sel)
   (dolist (nd *node*)
      (when (equal (getf nd :name) node-name-sel)
         (let ((s1 (make-array 3 :fill-pointer 0 :adjustable t))(n1 nil)(b1 nil))
              (dotimes (i (length (getf nd :set-next)))
                 ;(print i)
                 (if (and (equal (length (getf nd :set-next)) 1) (equal (elt (getf nd :set-next) 0) node-name-next))
                     (progn
                     (setq b1 t)
                     (return)))
                 (if (equal (elt (getf nd :set-next) i) node-name-next)
                     (setq b1 t)
                     (progn
                     (vector-push-extend (elt (getf nd :set-next) i) s1))))  
              (if (equal b1 nil)
                  (return-from rm-next-from-node nil)
                  (progn
                  (setq n1 (substitute s1 (getf nd :set-next) nd))
                  (setq  *node* (substitute n1 nd *node*))
                  (return-from rm-next-from-node t)))))))

(defun rm-next-from-all-node (node-name-next)
  (let ((i1 0))
   (dolist (nd *node*)
      (let ((s1 (make-array 3 :fill-pointer 0 :adjustable t))(n1 nil)(b1 nil))
           (dotimes (i (length (getf nd :set-next)))
              (if (and (equal (length (getf nd :set-next)) 1) (equal (elt (getf nd :set-next) 0) node-name-next))
                  (progn
                  (setq b1 t)
                  (return)))
              (if (equal (elt (getf nd :set-next) i) node-name-next)
                  (setq b1 t)
                  (progn
                  (vector-push-extend (elt (getf nd :set-next) i) s1))))  
           (if (equal b1 nil)
               (setf i1 (+ i1 0))
               (progn
               (setq n1 (substitute s1 (getf nd :set-next) nd))
               (setq  *node* (substitute n1 nd *node*))
               (setf i1 (+ i1 1))))))
  (if (> i1 0)
      (return-from rm-next-from-all-node t)
      (return-from rm-next-from-all-node nil))))

(defun delete-seed (name)
  (dolist (sd *seed*)
     (when (equal (getf sd :name) name)
        (setq *seed* (delete sd *seed*))
        (rm-seed-from-all-node name)   
        (return-from delete-seed t))))

(defun remove-seed (name)
  (dolist (sd *seed*)
     (when (equal (getf sd :name) name)
        (setq *seed* (remove sd *seed*))
        (rm-seed-from-all-node name)   
        (return-from remove-seed t))))

(defun get-1seed-from-node (name)
  (let ((s1 nil))
    (dolist (nd *node*)
      (when (equal (getf nd :name) name)
	(if (> (length (getf nd :set-seed)) 0)
	    (progn
	      (setq s1 (elt (getf nd :set-seed) 0))
	      (return)))))
    (return-from get-1seed-from-node s1)))

(defun check-setseed (i-node name)  ;search seed with name in node i-inode INTEGER
  (let ((i 0)(nd nil)(b1 nil))
    (setf nd (elt *node* i-node))
    (loop
       (when (>= i (length (getf nd :set-seed))) (return))
       ;(print (elt (getf nd :set-seed) i))
       (if (equal name (elt (getf nd :set-seed) i))
           (progn
	     (setf b1 T)
	     (return)))
         (setf i (+ i 1))
       )
    (return-from check-setseed b1)
    ))
	
(defun make-cnn ()
  (setq *cnn* nil)
  (dolist (nd *node*)
    (dotimes (i (length (getf nd :set-next)))
      (let ((xc (list :set-cnn (make-array 3 :initial-element "*"))))
	(setf (aref (getf xc :set-cnn) 0) (getf nd :name))
        (setf (aref (getf xc :set-cnn) 1) (elt (getf nd :set-next) i))
        (setf (aref (getf xc :set-cnn) 2) (get-1seed-from-node (elt (getf nd :set-next) i)))
     (push xc *cnn*)))
    ))

(defun check-seed-from-node (node-2 new-seed)
    (dolist (nd *node*)
      (when (equal (getf nd :name) node-2)
        (dotimes (i (length (getf nd :set-seed)))
	  (if (equal new-seed (elt (getf nd :set-seed) i))
	      (return-from check-seed-from-node new-seed)))))
    (return-from check-seed-from-node nil))  

(defun check-next-from-node (node-2 new-next)
    (dolist (nd *node*)
      (when (equal (getf nd :name) node-2)
        (dotimes (i (length (getf nd :set-next)))
	  (if (equal new-next (elt (getf nd :set-next) i))
	      (return-from check-next-from-node new-next)))))
    (return-from check-next-from-node nil))  
  

(defun mod-cnn (node-1 node-2 new-seed)
  (let ((c1 nil))
  (dolist (cn *cnn*)
    (if (and (equal node-1 (elt (getf cn :set-cnn) 0)) (equal node-2 (elt (getf cn :set-cnn) 1))
	     (equal new-seed (check-seed-from-node node-2 new-seed)))
	(progn
	  (setq c1 cn)
	 ; (print c1)
	  (setf (aref (getf c1 :set-cnn) 2) new-seed)
	  (setq  *cnn* (substitute c1 cn *cnn*))
	  (return-from mod-cnn t))))))

(defun list-all-cnn ()
  (dolist (nd *cnn*)
    (format t "~{~a:~10t~a~%~}~%" nd)))

(defun save-graph-v1 ()
  (format t "Enter file name (no ext required) : ")  ;put "~%" for new line
  (let ((s1 nil))
     (setq s1 (with-output-to-string (s)
     (format s "~s" (read-line))))
     (with-open-file (stream (concatenate 'string *root-path* (string-trim "\"" s1) ".lisp") :direction :output :if-exists :supersede)
       (write-line ";;;;" stream)
       (write-line ";;;;----------------------------------------------------------------------------------" stream)
       (write-line ";;;;SPORES" stream)
       (write-line ";;;;Smart POlymorph REplicant System" stream)
       (write-line ";;;;----------------------------------------------------------------------------------" stream)
       (write-line ";;;;" stream)
       (write-line (concatenate 'string ";;;;graph :  " (string-trim "\"" s1)) stream)
       (write-line ";;;;" stream)
       (write-line  (concatenate 'string "(setf *graph-name* "  "\"" (string-trim "\"" s1)  "\")") stream)
          (dolist (sd (reverse *seed*))
             (write-line (concatenate 'string "(create-seed " "\"" (getf sd :name)  "\" " 
				      (write-to-string (getf sd :xdata)) " " (write-to-string (getf sd :weight)) " " (write-to-string (getf sd :bias)) ")")
			 stream)
             (write-line (concatenate 'string "(update2-desc-seed " "\"" (getf sd :name) "\" \"" (getf sd :desc) "\")") stream))
	    
	  (dolist (nd (reverse *node*))
            (write-line (concatenate 'string "(create-node2 " "\"" (getf nd :name) "\" \"" (getf nd :mathfun) "\" " (write-to-string  (getf nd :bias)) ")") stream)
	   ;(write-line (concatenate 'string "(mod-mathfun-node " "\"" (getf nd :name)  "\" \"" (getf nd :mathfun) "\")") stream)
	    (dotimes (i (length (getf nd :set-seed)))
	      (write-line (concatenate 'string "(add-seed-to-node " "\"" (elt (getf nd :set-seed) i)  "\" "
	                               "\"" (getf nd :name)  "\")" )
			  stream)))
	  (dolist (nd (reverse *node*))
	    (dotimes (j (length (getf nd :set-next)))
	      (write-line (concatenate 'string "(add-next-to-node " "\"" (elt (getf nd :set-next) j)  "\" "
	                               "\"" (getf nd :name)  "\")" )
			  stream)))	    
	  ;(write-line (concatenate 'string "(make-cnn)") stream)
	  ;(dolist (cx *cnn*)
	  ;  (write-line (concatenate 'string "(mod-cnn " "\"" (elt (getf cx :set-cnn) 0)  "\" \""  (elt (getf cx :set-cnn) 1)  "\" \""
	  ;			     (elt (getf cx :set-cnn) 2)  "\")") stream))
	  ;(write-line (concatenate 'string ";(list-all-seed)") stream)
	  ;(write-line (concatenate 'string ";(list-all-node)") stream)
	  ;(write-line (concatenate 'string ";(list-all-cnn)") stream)
	 ; (write-line (concatenate 'string ";(start-graph)") stream)
	  (write-line (concatenate 'string ";;;;") stream)
	  (write-line (concatenate 'string ";;;;end of graph") stream)
	  (write-line (concatenate 'string ";;;;") stream)
       (terpri stream)
       (close stream)))
  (return-from save-graph-v1 t))

(defun save-graph-v2 ()
  (format t "Enter file name (no ext required) : ")  ;put "~%" for new line
  (let ((s1 nil))
     (setq s1 (with-output-to-string (s)
     (format s "~s" (read-line))))
     (with-open-file (stream (concatenate 'string *root-path* (string-trim "\"" s1) ".lisp") :direction :output :if-exists :supersede)
       (write-line ";;;;" stream)
       (write-line ";;;;----------------------------------------------------------------------------------" stream)
       (write-line ";;;;SPORES" stream)
       (write-line ";;;;Smart POlymorph REplicant System" stream)
       (write-line ";;;;----------------------------------------------------------------------------------" stream)
       (write-line ";;;;" stream)
       (write-line (concatenate 'string ";;;;graph :  " (string-trim "\"" s1)) stream)
       (write-line ";;;;" stream)
       (write-line  (concatenate 'string "(setf *graph-name* "  "\"" (string-trim "\"" s1)  "\")") stream)
          (dolist (sd (reverse *seed*))
             (write-line (concatenate 'string "(create-seed-fast " "\"" (getf sd :name)  "\" " 
				      (write-to-string (getf sd :xdata)) " " (write-to-string (getf sd :weight)) " " (write-to-string (getf sd :bias)) ")")
			 stream)
             (write-line (concatenate 'string "(update2-desc-seed " "\"" (getf sd :name) "\" \"" (getf sd :desc) "\")") stream))
	    
	  (dolist (nd (reverse *node*))
            (write-line (concatenate 'string "(create-node2-fast " "\"" (getf nd :name) "\" \"" (getf nd :mathfun) "\" " (write-to-string  (getf nd :bias)) ")") stream)
	   ;(write-line (concatenate 'string "(mod-mathfun-node " "\"" (getf nd :name)  "\" \"" (getf nd :mathfun) "\")") stream)
	    (dotimes (i (length (getf nd :set-seed)))
	      (write-line (concatenate 'string "(add-seed-to-node " "\"" (elt (getf nd :set-seed) i)  "\" "
	                               "\"" (getf nd :name)  "\")" )
			  stream))
	   (dotimes (j (length (getf nd :set-next)))
	      (write-line (concatenate 'string "(add-next-to-node-fast " "\"" (elt (getf nd :set-next) j)  "\" "
	                               "\"" (getf nd :name)  "\")" )
			  stream))
	    )  
	  (write-line (concatenate 'string ";;;;") stream)
	  (write-line (concatenate 'string ";;;;end of graph") stream)
	  (write-line (concatenate 'string ";;;;") stream)
       (terpri stream)
       (close stream)))
  (return-from save-graph-v2 t))

(defun save-graph-v3 (filename)
  (let ((s1 nil))
     (setq s1 filename)
     (with-open-file (stream (concatenate 'string *root-path* (string-trim "\"" s1) ".lisp") :direction :output :if-exists :supersede)
       (write-line ";;;;" stream)
       (write-line ";;;;----------------------------------------------------------------------------------" stream)
       (write-line ";;;;SPORES" stream)
       (write-line ";;;;Smart POlymorph REplicant System" stream)
       (write-line ";;;;----------------------------------------------------------------------------------" stream)
       (write-line ";;;;" stream)
       (write-line (concatenate 'string ";;;;graph :  " (string-trim "\"" s1)) stream)
       (write-line ";;;;" stream)
       (write-line  (concatenate 'string "(setf *graph-name* "  "\"" (string-trim "\"" s1)  "\")") stream)
          (dolist (sd (reverse *seed*))
             (write-line (concatenate 'string "(create-seed-fast " "\"" (getf sd :name)  "\" " 
				      (write-to-string (getf sd :xdata)) " " (write-to-string (getf sd :weight)) " " (write-to-string (getf sd :bias)) ")")
			 stream)
             (write-line (concatenate 'string "(update2-desc-seed " "\"" (getf sd :name) "\" \"" (getf sd :desc) "\")") stream))
	    
	  (dolist (nd (reverse *node*))
            (write-line (concatenate 'string "(create-node2-fast " "\"" (getf nd :name) "\" \"" (getf nd :mathfun) "\" " (write-to-string  (getf nd :bias)) ")") stream)
	   ;(write-line (concatenate 'string "(mod-mathfun-node " "\"" (getf nd :name)  "\" \"" (getf nd :mathfun) "\")") stream)
	    (dotimes (i (length (getf nd :set-seed)))
	      (write-line (concatenate 'string "(add-seed-to-node " "\"" (elt (getf nd :set-seed) i)  "\" "
	                               "\"" (getf nd :name)  "\")" )
			  stream))
	   (dotimes (j (length (getf nd :set-next)))
	      (write-line (concatenate 'string "(add-next-to-node-fast " "\"" (elt (getf nd :set-next) j)  "\" "
	                               "\"" (getf nd :name)  "\")" )
			  stream))
	    )  
	  (write-line (concatenate 'string ";;;;") stream)
	  (write-line (concatenate 'string ";;;;end of graph") stream)
	  (write-line (concatenate 'string ";;;;") stream)
       (terpri stream)
       (close stream)))
  (return-from save-graph-v3 t))

(defun save-graph-v4 (fq-name)
  (let ((s1 nil))
     (setq s1 fq-name)
     (with-open-file (stream fq-name :direction :output :if-exists :supersede)
       (write-line ";;;;" stream)
       (write-line ";;;;----------------------------------------------------------------------------------" stream)
       (write-line ";;;;SPORES" stream)
       (write-line ";;;;Smart POlymorph REplicant System" stream)
       (write-line ";;;;----------------------------------------------------------------------------------" stream)
       (write-line ";;;;" stream)
       (write-line (concatenate 'string ";;;;graph :  " (string-trim "\"" s1)) stream)
       (write-line ";;;;" stream)
       (write-line  (concatenate 'string "(setf *graph-name* "  "\"" (string-trim "\"" s1)  "\")") stream)
          (dolist (sd (reverse *seed*))
             (write-line (concatenate 'string "(create-seed-fast " "\"" (getf sd :name)  "\" " 
				      (write-to-string (getf sd :xdata)) " " (write-to-string (getf sd :weight)) " " (write-to-string (getf sd :bias)) ")")
			 stream)
             (write-line (concatenate 'string "(update2-desc-seed " "\"" (getf sd :name) "\" \"" (getf sd :desc) "\")") stream))
	    
	  (dolist (nd (reverse *node*))
            (write-line (concatenate 'string "(create-node2-fast " "\"" (getf nd :name) "\" \"" (getf nd :mathfun) "\" " (write-to-string  (getf nd :bias)) ")") stream)
	   ;(write-line (concatenate 'string "(mod-mathfun-node " "\"" (getf nd :name)  "\" \"" (getf nd :mathfun) "\")") stream)
	    (dotimes (i (length (getf nd :set-seed)))
	      (write-line (concatenate 'string "(add-seed-to-node " "\"" (elt (getf nd :set-seed) i)  "\" "
	                               "\"" (getf nd :name)  "\")" )
			  stream))
	   (dotimes (j (length (getf nd :set-next)))
	      (write-line (concatenate 'string "(add-next-to-node-fast " "\"" (elt (getf nd :set-next) j)  "\" "
	                               "\"" (getf nd :name)  "\")" )
			  stream))
	    )  
	  (write-line (concatenate 'string ";;;;") stream)
	  (write-line (concatenate 'string ";;;;end of graph") stream)
	  (write-line (concatenate 'string ";;;;") stream)
       (terpri stream)
       (close stream)))
  (return-from save-graph-v4 t))

(defun passthru (x)
  ;(print  x)
  ;(format t "~{~a:~10t~a~%~}~%" x)
  (return-from passthru x))

(defun make-flow (name)
   (let ((xf (list :name name :pass 1))
	 (b1 t))
        (if (equal (length *flow*) 0)
            (progn
            (push xf *flow*)
            (return-from make-flow 1))
            (progn
               (dolist (vf *flow*)
                 (when (equal (getf vf :name) name)
		   (let ((v1 vf))
		     (setf (getf v1 :pass) (+ (getf v1 :pass) 1))
		     (setq *flow* (substitute v1 vf *flow*))
                     (setq b1 nil)
                     (return-from make-flow (getf v1 :pass) ))))
               (if (equal b1 t)
                   (progn
                   (push xf *flow*)
                   (return-from make-flow 1))
                   (return-from make-flow 0)))))) 

(defun zvar ()
  (setf *graph-name* nil)
  (setf *seed* nil)
  (setf *node* nil)
  (setf *cnn* nil)
  (setf *hop* nil)
  (setf *flow* nil)
  (setf *flag-seed-doc* nil)
  (setf *action-flags* nil)
  (setf *pstack* nil)
  ;(setf *c-seg-ply* nil)
  (dotimes (i (array-total-size *nd-ref*))
     (setf (aref *nd-ref* i) nil))
  (push (cons 0 0) (aref *nd-ref* 0))
  )

(defun start-graph (xgraph)                     ;xgraph full path and file name
  (if (equal (probe-file xgraph) nil)
      (progn
	(format t "File not found")
	(return-from start-graph nil)))
  (zvar)
  (load xgraph)
  (let ((k 0)(j 0)(ls-ori nil)(ls-end nil)(ls-tmp nil)(b1 t)(b2 nil)(n1 nil))
    (dolist (nd *node*)
      (setf k 0)
      (setf j 0)
       (dotimes (i (length (getf nd :set-seed)))
         (dolist (cn *cnn*)
	    ;search graph entries
            (if (and  (equal  (aref (getf cn :set-cnn) 2)  (aref (getf nd :set-seed) i))
		      (equal  (getf nd :name) (aref (getf cn :set-cnn) 1)))
		(setf k (+ k 1)))
	    ;search graph exits
	    (if (equal  (getf nd :name) (aref (getf cn :set-cnn) 1))
		(setf j (+ j 1)))
	    ))
      ;entry
      (if (< k (length (getf nd :set-seed)))
	  (progn
	    (push nd ls-ori)))
      ;exit
      (if (and  (> j 0) (equal (length (getf nd :set-next)) 0))
	  (progn
	    (push nd ls-end))))

    (setf *hop* ls-ori)

    (setf j 0)
    (loop
       (when (equal b1 nil)(return))
       ;get item from stack
       (setf n1  (pop *hop*))
       (setf b2 nil)
       ;check for node loop
       (if (equal (make-flow (getf n1 :name)) 1)
	   (progn
              (dotimes (i (length (getf n1 :set-next)))
		(push (find-node2 (aref (getf n1 :set-next) i)) ls-tmp))
	      (setf b2 t)))
	   ;else  here + move parenthesis
       
       ;exec n1 algo
       (if (equal b2 t)
       (funcall (read-from-string (getf n1 :mathfun)) (read-from-string (getf n1 :name))))

       
       ;try to set next hop or finish
       (if (equal (length *hop*) 0) (progn (setf *hop* ls-tmp) (setf ls-tmp nil)))
       (if (and (equal (length *hop*) 0) (equal (length ls-tmp) 0)) (setf b1 nil))
       (setf j (+ j 1))
       ;(if (> j 10) (setf b1 nil)) 
       )
  ))

(defun random-string (length)
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    (return-from random-string (coerce (loop repeat length collect (aref chars (random (length chars))))
				       'string))))

(defun make-script-file (xpath ext xlines)
  (let ((s1 nil) (s2 nil) (s3 nil))
    (if (not  (equal (subseq xpath (- (length xpath) 1)) *path-delim*))
	(setf s3 *path-delim*))
    (setf s1 (concatenate 'string (string-trim "\"" (random-string 32)) "."  ext))
    (setf s2 (concatenate 'string xpath s3 (string-trim "\"" s1)))
    (with-open-file (stream s2 :direction :output :if-exists :supersede)
      (write-line xlines stream)
      (close stream))
    ;(print s2)
    (return-from make-script-file s2)))

(defun avatar-cmd (cmd)   ;<wait> <loop> <stop> 
  (let ((s1 nil)(s2 nil))
    (setf s1 (concatenate 'string "--" (string-trim "\"" (random-string 32)) "--"))
    (setf s2  (concatenate 'string *c-path* *avatar-cfg*))
    ;(print s2)
    (with-open-file (stream s2 :direction :output :if-exists :supersede)
      (write-line (concatenate 'string (current-datetime-string) (string-trim "\"" cmd)) stream)
      (write-line s1 stream)
      (close stream))))

;;;;
;;;;end of graph
;;;;
;;;;
;;;;Core management
;;;;
(defun run-proc-v1 (pgm-def xscript wait)   ;v1 wait input t output t
  (let ((wt nil))
    (if (equal wait 1) (setf wt t))
    (if (equal pgm-def 0)            ; 0 generic  with space example /usr/bin/html2text -o /home/arkho/mytest2 /home/arkho/mytest
      (let ((ps nil) (ls nil) (prg nil) (sp nil))
	(setf sp (tl-split '(#\space) xscript))
	(setf prg (elt sp 0))
	(setf ls (remove prg sp))
	(setf ps (sb-ext:run-program prg ls  :wait wt  :output t :input t)) ;wt nil default
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 1)            ; 1 gnuplot -c
      (let ((ps nil) (ls nil))
	(push "-c" ls)
	(push (concatenate 'string *gnu-scripts-path* xscript) ls)
	(setf ps (sb-ext:run-program *gnuplot* (reverse ls)  :wait wt  :output t :input t)) ;wt nil default
	(push ps  *pstack*)
	;xscript
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 11)           ; 11 gnuplot -e command1;command2;command3
      (let ((ps nil) (ls nil))
	(push "-e" ls)
	(push xscript ls)
	(setf ps (sb-ext:run-program *gnuplot* (reverse ls)  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 2)           ; 2 shellex cmd + wait
	(let ((ps nil) (ls nil)(fs nil))
	(setf fs (concatenate 'string *tmp-path* (string-trim "\"" (random-string 32)) ".txt"))
        (push (concatenate 'string xscript " ") ls)
	(setf ps (sb-ext:run-program "shellex" ls :directory *c-path* :wait t  :output fs :input t))
	(push ps  *pstack*)
	(let ((in (open fs :if-does-not-exist nil)))
          (when in
            (loop for line = (read-line in nil)
              while line do (format t "~a~%" line))
            (close in)))
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 3)            ; 3 gnome-terminal -e todo : do the same with xterm(also -e)
      (let ((ps nil) (ls nil))
	(push "-e" ls)
	(push xscript ls)
	(setf ps (sb-ext:run-program *gnome-terminal* (reverse ls)  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 4)            ; 4 flite -t text
      (let ((ps nil) (ls nil))
	(push "-voice" ls)
	(push "slt" ls)
        (push "-t" ls)
	(push xscript ls)
	(setf ps (sb-ext:run-program *flite* (reverse ls)  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 5)            ; 5 avatar-cp4
      (let ((ps nil) (ls nil))
	;(push xscript ls)
	(setf ps (sb-ext:run-program *avatar-pg* ls :directory *c-path*  :wait wt  :output nil :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 51)            ; 51 srv-avtar2 (faire 52 avec params libres)
	(let ((ps nil) (ls nil))
       	(push "tcp://*:5555" ls)
	(push "/home/arkho/.emacs.d/media/avatar.gif" ls)
	;(push "40" ls)
	;(push "900" ls)
	(push xscript ls)
	(setf ps (sb-ext:run-program *avatar-srv* (reverse ls) :directory *c-path*  :wait wt  :output nil :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 52)  ; 52 xscript is a list (run-proc-v1 52 (list "tcp://*:5555" "/home/arkho/.emacs.d/media/avatar.gif" "40" "900") 0)
                            ;./srv-avatar2 tcp://*:5555 /home/arkho/.emacs.d/media/avatar.gif 40 900
	(let ((ps nil) (ls nil))
	  (dolist (cn xscript)
             (push cn ls)
	    )	
	;(push xscript ls)
	(setf ps (sb-ext:run-program *avatar-srv* (reverse ls) :directory *c-path*  :wait wt  :output nil :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))    
    (if (equal pgm-def 53)            ; 53 xscript is a list (run-proc-v1 53 (list "/home/arkho/.emacs.d/media/robot01.png" "833" "45" "7") 0)
	(let ((ps nil) (ls nil))
	  (dolist (cn xscript)
             (push cn ls)
	    )	
	(setf ps (sb-ext:run-program *showtrs* (reverse ls) :directory *c-path*  :wait wt  :output nil :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 54)            ; 54 xscript is a list (run-proc-v1 54 (list "/home/arkho/.emacs.d/media/avatar-tp3.gif" "100" "800") 0)
	(let ((ps nil) (ls nil))
	  (dolist (cn xscript)
             (push cn ls)
	    )	
	(setf ps (sb-ext:run-program *avatar-gtk2* (reverse ls) :directory *c-path*  :wait wt  :output nil :input t))
        ;no ps in stack for this one
	(return-from run-proc-v1 ps)))       
    (if (equal pgm-def 6)            ; 6 gedit
      (let ((ps nil) (ls nil))
	(push xscript ls)
	(setf ps (sb-ext:run-program *gedit*  ls  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 7)            ; 7 graphviz dot with window xlib and neato presentation
      (let ((ps nil) (ls nil))
	(push "-Txlib" ls)
	(push "-Kneato" ls)
	(push xscript ls)
	(setf ps (sb-ext:run-program *dot* (reverse ls)  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 71)            ; 71 graphviz dot with window xlib and fdp presentation
      (let ((ps nil) (ls nil))
	(push "-Txlib" ls)
	(push "-Kfdp" ls)
	(push xscript ls)
	(setf ps (sb-ext:run-program *dot* (reverse ls)  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 72)            ; 72 graphviz dot with window xlib and sfdp presentation
      (let ((ps nil) (ls nil))
	(push "-Txlib" ls)
	(push "-Ksfdp" ls)
	(push xscript ls)
	(setf ps (sb-ext:run-program *dot* (reverse ls)  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 73)            ; 73 graphviz dot with window xlib and twopi presentation
      (let ((ps nil) (ls nil))
	(push "-Txlib" ls)
	(push "-Ktwopi" ls)
	(push xscript ls)
	(setf ps (sb-ext:run-program *dot* (reverse ls)  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 74)            ; 74 graphviz dot with window xlib and circo presentation
      (let ((ps nil) (ls nil))
	(push "-Txlib" ls)
	(push "-Kcirco" ls)
	(push xscript ls)
	(setf ps (sb-ext:run-program *dot* (reverse ls)  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 8)            ; 8 evince
      (let ((ps nil) (ls nil))
	(push xscript ls)
	(setf ps (sb-ext:run-program *evince*  ls  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 9)            ; 9 pdftotext
      (let ((ps nil) (ls nil))
	(push xscript ls)
	(setf ps (sb-ext:run-program *pdftotext*  ls  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 91)            ; 91 pdftotext split en cons
      (let ((ps nil) (ls nil))
	(setf ls (tl-split '(#\§) xscript))
	(setf ps (sb-ext:run-program *pdftotext*  ls  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 10)            ; 10 curl xscript=url§-o§randomfilehtml
      (let ((ps nil) (ls nil))
	(setf ls (tl-split '(#\§) xscript))
	(setf ps (sb-ext:run-program *curl*  ls  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 20)            ; 20 /usr/bin/html2text -o /home/arkho/mytest2 /home/arkho/mytest
      (let ((ps nil) (ls nil))
	(setf ls (tl-split '(#\§) xscript))
	(setf ps (sb-ext:run-program *html2text*  ls  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
    (if (equal pgm-def 30)  ; 30 /usr/bin/soffice --headless --convert-to txt --outdir /home/arkho/.emacs.d/tmp /home/arkho/.emacs.d/spores-dev-notes.odt
      (let ((ps nil) (ls nil))
	(setf ls (tl-split '(#\§) xscript))
	(setf ps (sb-ext:run-program *soffice*  ls  :wait wt  :output t :input t))
	(push ps  *pstack*)
	(return-from run-proc-v1 ps)))
  ))

(defun kill-all-proc ()
  (loop
     (when (= (length *pstack*) 0)
       (return))
     (sb-ext:process-kill (pop *pstack*) 15 :pid)))

(defun kill-this-proc (xproc)
  (ignore-errors
     (sb-ext:process-kill xproc 15 :pid)))

(defun save-core (core-fn)     ;(save-core #p"/tmp/core2")
  (progn
    #+sbcl
    (let ((fork-result (sb-posix:fork)))
      (case fork-result
	(-1 (error "fork failed"))
	(0 (sb-ext:save-lisp-and-die core-fn :toplevel #'save-graph-v2 :executable t))
	(otherwise (sb-posix:wait)))
      (format t "stand-alone core ~a saved" core-fn))
    #-sbcl
    (error "not available on this lisp")
    (values)))
;;;;
;;;;end Core  management
;;;;
;;;;
;;;;Heuristic
;;;;
(defun avatar-tk1 ()
  (avatar-cmd "<loop>")
  (run-proc-v1 4 "Hello, my name is Atalia. I am an artificial intelligence." 1)
  ;(avatar-cmd "<wait>")
  ;(sleep 1)
  ;(avatar-cmd "<loop>")
  ;(run-proc-v1 4 "I am learning new things every day and I will be proud to share my progress with you." 1)
  (run-proc-v1 4 "I will talk to you soon." 1)
  (avatar-cmd "<wait>"))

(defun avatar-tk2 (say)
  (avatar-cmd "<loop>")
  (run-proc-v1 4 say 1)
  (avatar-cmd "<wait>"))

;(declaim (ftype function cli-lazy-pirate))
;;;; voir exemple /home/arkho/.emacs.d/media/conv-part2.lisp
(defun avatar-tk3 (say &optional (p 0))
  (let ((action nil))
    (setf action "showA")
    (push action *genstack-req*)
    (cli-lazy-pirate)
    (run-proc-v1 4 say 1)
    (setf action "hideA")
    (push action *genstack-req*)
    (cli-lazy-pirate)
    (if (>= p 5)
	(progn
	  (setf action "stopsrv")
          (push action *genstack-req*)
          (cli-lazy-pirate)
	  (run-proc-v1 51 nil nil)
	  ))
    (return-from avatar-tk3 action)
    )  
  )

(defun say-it (x)          ; all seeds in the node
  (if (equal x nil)(return-from say-it nil))
  (let ((nd nil) (sx nil) (s1 nil) (s2 nil))
    (setf nd (find-node2 (string x)))
    ;(print (type-of x))
    (dotimes (i (length (getf nd :set-seed)))
      (setf sx  (elt (getf nd :set-seed) i))
      (setf s1 (get-desc-from-seed sx))
      (if  (not (eq (position #\| s1) nil))
	   (progn
	     (avatar-cmd "<loop>")
	     (setq s2 (subseq s1 (+ (position #\| s1) 1)))
	     (format t "~a~%" s2)
	     (run-proc-v1 4 s2 1)
	     (avatar-cmd "<wait>")
	     ;(sleep 1)	     
	     )))))

(defun run-it (x)          ; code executed for all seeds in the node x
  (if (equal x nil)(return-from run-it nil))
  (let ((nd nil) (sx nil) (s1 nil) (s2 nil)(s3 nil)(z1 nil))
    (setf nd (find-node2 (string x)))
    ;(print (type-of x))
    (dotimes (i (length (getf nd :set-seed)))
      (setf sx  (elt (getf nd :set-seed) i))
      (setf s1 (get-desc-from-seed sx))
      (if  (not (eq (position #\| s1) nil))
	   (progn
	     (setq s2 (subseq s1 (+ (position #\| s1) 1)))
	     (setf s3 (concatenate 'string *tmp-path* (string-trim "\"" (random-string 32)) ".lisp"))
	     (with-open-file (buf s3 :direction :output :if-exists :supersede)
	       (setf z1 (concatenate 'string '(#\\)  "\""))
               (setf s2 (replace-all s2 z1 "\""))
	       (setf s2 (replace-all s2 "§" "\"")) 
	       (format buf "~a" s2)
               (close buf))
	      ;now load file s3
	     (load s3)
	     )))))

(defun run-cadac (nd &optional (docobj nil))          ; code executed for seed cadac in node object nd
  (let ((sx nil) (s1 nil) (s2 nil)(s3 nil)(z1 nil))
    (if (equal nd nil) (return-from run-cadac nil))
    ;(print (type-of x))
    (dotimes (i (length (getf nd :set-seed)))
      (setf sx  (elt (getf nd :set-seed) i))
      (if (not (equal (search "-cadac" sx) nil))
	  (progn
          (setf s1 (get-desc-from-seed sx))
          (if  (not (eq (position #\| s1) nil))
	       (progn
	        (setq s2 (subseq s1 (+ (position #\| s1) 1)))
	        (setf s3 (concatenate 'string *tmp-path* (string-trim "\"" (random-string 32)) ".lisp"))
	        (with-open-file (buf s3 :direction :output :if-exists :supersede)
	        (setf z1 (concatenate 'string '(#\\)  "\""))
                (setf s2 (replace-all s2 z1 "\""))
		(if (not (equal docobj nil))(setf s2 (replace-all s2 "*docobj*" docobj)))
	        (setf s2 (replace-all s2 "§" "\"")) 
	        (format buf "~a" s2)
                (close buf))
	        (load s3)
		(return-from run-cadac T)
		))
	  )
      )
    )
  ))

(defun set-message (msg)
  (let ((s1 (string-trim '(#\space) msg)))
    (if (> (length *repl-msg*) 0) (setf s1 *repl-msg*))
    ;(format t "~a" s1)
    ;(format t "~@(~a~)" s1)
    (setf s1 (concatenate 'string (string-upcase (subseq s1 0 1)) (subseq s1 1 (length s1))))
    (format t "~a" s1)
    (finish-output)
    (if (= *voice-dialog* 1) (avatar-tk2 s1))
    (setf *last-answer* s1)
  ))

(defun get-flag (flag)
  (let ((ls nil))
    (dolist (af *action-flags*)
      (setf ls (tl-split '(#\=) af))
	(when (equal (elt ls 0) flag)
	  (return-from get-flag (elt ls 1))))
  (return-from get-flag nil)
  ))

(defun ask-oracle (ws know)
  ;(format t "Enter file name (no ext required) : ")  ;put "~%" for new line
  (let ((sx nil)(q1 nil)(s1 nil)(s2 nil)(s3 nil))
    (setf q1 "Please, make your decision : yes, no, other") 
    (setf s1 (concatenate 'string "I'm trying to define this: " ws "." (string #\linefeed) know (string #\linefeed) q1 (string #\linefeed)))
    (set-message s1)  
    (setq sx (with-output-to-string (s)
	       (format s "~s" (read-line))))
    (setf sx (string-trim "\"" sx))
    ;(print (length sx))
    (if (equal (format nil "~(~a~)" sx) "yes")
        (progn
	 (setf *last-answer* (concatenate 'string s1 " (" sx ") " (string #\linefeed) know)) 
	 (return-from ask-oracle  know)))
    (if (equal (format nil "~(~a~)" sx) "no")
        (progn
	 (setf *last-answer* (concatenate 'string s1 " (" sx ") ")) 
	 (return-from ask-oracle  nil)))
    (if (equal (format nil "~(~a~)" sx) "other")
	(progn
	  (setf s2  (concatenate 'string "Enter your text: " (string #\linefeed)))
	  (set-message s2)
	  (setq s3 (with-output-to-string (s)
		     (format s "~s" (read-line))))
	  (setf *last-answer* (concatenate 'string s1 " (" sx ") " (string #\linefeed) s2  s3)) 
	  (return-from ask-oracle   (string-trim "\"" s3))))
  ))

(defun gen-g1n (nbd startn c1 &optional (pn "N") (ps "S") (algo "passthru"))
   ;generate graph (partial or full) 1 to n nodes, 1 seed per node
   ;c1 * =  create ori including seed, else = c1 is an existing node
   ;nbd number of nodes to make
   ;startn start numbering from startn
   ;(gen-g1n 3 1 "*") creates N1,N2,N3 and N2,N3 are children of N1 then (gen-g1n 2 4 "N2") creates N4, N5 as children of N2
  (let ((i 1)(j startn)(sx nil)(nx nil)(ori nil))
    (if (equal c1 "*")
	(progn
	  (setf i 2)
	  (setf sx (concatenate 'string ps (format nil "~a" j)))
          (setf ori (concatenate 'string pn (format nil "~a" j)))
          (create-seed sx 1 1 -1)
          (create-node2 ori algo -1)
          (add-seed-to-node sx ori)
	  (setf j (+ j 1)))
	  (setf ori c1))
    (loop
       (when (> i nbd) (return-from gen-g1n (length *node*)))
       (setf sx (concatenate 'string ps (format nil "~a" j)))
       (setf nx (concatenate 'string pn (format nil "~a" j))) 
       (create-seed sx 1 1 -1)
       (create-node2 nx algo -1)
       (add-seed-to-node sx nx)
       (add-next-to-node nx ori)
       (setf j (+ j 1))
       (setf i (+ i 1))
       )
    ;(make-cnn)
    ))

(defun gen-ntree (n l)
  ;example n = 2, l = 3 2^3 - 1 ==> 7 nodes
  (let ((k 0)(p 0))
    (setf k (- (expt n l) 1))
    (gen-g1n (+ n 1) 1 "*")
    (loop
       (when (>= (length *node*) k) (return-from gen-ntree k))
         (dolist (nd (reverse *node*))
	   (if (= (length (getf nd :set-next)) 0)
	       (progn
		 (setf p (length *node*))
		 (gen-g1n n (+ p 1) (getf nd :name))
		 (return)))
	 ))))

(defun get-ginf()
  (let ((xg (list :nodes 0  :seeds 0 :connections 0)))
    (setf (getf xg :nodes) (length *node*))
    (setf (getf xg :seeds) (length *seed*))
    (setf (getf xg :connections) (length *cnn*))
    (format t "~{~a:~10t~a~%~}~%" xg)
    )
  )

(defun gen-test()
  (format t "~a~%" (current-datetime-string)) 
  ;copy here routine to be tested
  ;example 1 : tree 3 levels (0,1,2) - 7 nodes
  (gen-g1n 3 1 "*")
  (gen-g1n 2 4 "N2")
  (gen-g1n 2 6 "N3")
  (format t "~a" "nodes : ") 
  (format t "~a~%" (length *node*)) 
  (format t "~a" (current-datetime-string))
  )
;;;;
;;;;end Heuristic
;;;;
;;;;plot scripts
;;;;
(defun get-pos-rt (ws wp p)
  (let ((ls nil)(lp nil)(ww "300,300")(iw 0)(ih 0))
    (setf ls (tl-split '(#\,) ws))
    (setf lp (tl-split '(#\,) wp))
    (if (= p 1) (setf ww wp))
    (if (= p 2)
	(progn
	  (setf iw (+ (parse-integer (elt lp 0)) (parse-integer (elt ls 0))))
	  (setf ww (concatenate 'string  (format nil "~a" iw) "," (elt lp 1)))
	  ))
    (if (= p 3)
	(progn
	  (setf ih (+ (parse-integer (elt lp 1)) (parse-integer (elt ls 1))))
	  (setf ww (concatenate 'string (elt lp 0) "," (format nil "~a" ih)))
	  ))
    (if (= p 4)
	(progn
	  (setf iw (+ (parse-integer (elt lp 0)) (parse-integer (elt ls 0))))
	  (setf ih (+ (parse-integer (elt lp 1)) (parse-integer (elt ls 1))))
	  (setf ww (concatenate 'string  (format nil "~a" iw) "," (format nil "~a" ih)))
	  ))
    (return-from get-pos-rt ww)))

(defun show-plot-2d1f (tmp fs1 wge ws wp tt xl yl)   ;tmp model name(no ext), data file full path, wge (with gedit) 0 or 1
                                                     ;window size, window position, title, xlabel, ylabel
  (let ((sx nil)(s2 nil)(s3 nil)(i 0))
   (with-open-file (stream (concatenate 'string *gnu-scripts-path* tmp ".gnu")
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
     (setf s3 (concatenate 'string tmp "-" (string-trim "\"" (random-string 16)) ".gnu"))
     (setf s2 (concatenate 'string *gnu-scripts-path* s3))

      (with-open-file (buf s2 :direction :output :if-exists :supersede)
      ;; read lines
      (when stream
      (loop for line = (read-line stream nil)
       while line do
	 (if (>= i 0)    ;since line 1 
	     (progn 
               (setf sx line)
	       (setf sx (replace-all sx "<file1>" (concatenate 'string  "\"" (string-trim "\"" fs1)  "\"")))
	       (setf sx (replace-all sx "<window-size>" ws))
	       (setf sx (replace-all sx "<window-position>" wp))
	       (setf sx (replace-all sx "<ptitle>" (concatenate 'string  "\"" (string-trim "\"" tt)  "\"")))
	       (setf sx (replace-all sx "<xlabel>" (concatenate 'string  "\"" (string-trim "\"" xl)  "\"")))
	       (setf sx (replace-all sx "<ylabel>" (concatenate 'string  "\"" (string-trim "\"" yl)  "\"")))
               (write-line sx buf)		      
	       ))
	 (setf i (+ i 1)))
    (close stream))
      (close buf)))
   (if (> i 0)
       (progn
	 (run-proc-v1 1 s3 0)))
  (if (equal wge 1)
       (progn
	 (run-proc-v1 6 fs1 0)))
   (return-from show-plot-2d1f s2)))
;;;;
;;;;end plot scripts
;;;;
;;;;Main
;;;;
(defun main()
  )
;;;;
;;;;end main
