;;;;energy- water - file analysis
;;;;
;;;;
;;;;
(defun getd-zero-cons (fs)  ;fs is a file,  comma separated water ror energy- 
  (let ((s1 nil) (s2 nil) (ls nil)(i 0)(b1 nil))
   (with-open-file (stream fs
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
      (setf s2 (concatenate 'string *tmp-path* (string-trim "\"" (random-string 32)) ".csv"))
      (with-open-file (buf s2 :direction :output :if-exists :supersede)
      ;; read lines
      (when stream
      (loop for line = (read-line stream nil)
       while line do
	 (if (>= i 0)    ;att la premiere ligne est déja enlevé dans getd-water ou getd-energy
	     (progn 
	       (setf ls (tl-split '(#\,) line))
	       (setf b1 nil)
		     (if (>= (length ls) 3)
			(progn
			  (if (equal (length (string-trim '(#\space) (elt ls 2))) 0)
			      (setf b1 t)
			      (progn
			      (if (eq (read-from-string (elt ls 2)) 0) (setf b1 t))))
			  ))	       
	       (if (equal b1 t)   
		   (progn
		     (setf s1 (concatenate 'string (elt ls 0) ",0"))
		     (write-line s1 buf)
		     ))
	       ;(dolist (sx ls)
		 ;(print (elt ls 0)))
	       ;(format t "~a~%" line)
	       ;(print ls)
	       ))
	 (setf i (+ i 1)))
    (close stream))
    (close buf)))
   (return-from getd-zero-cons s2)))


(defun find-entry-dt (dt fs)           ;search datetime entry in file fs return true = found, nil = not found
  (let ((s1 nil)(ls nil)(i 0)(b1 nil))
   (with-open-file (stream fs
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
      (when stream
      (loop for line = (read-line stream nil)
       while line do
	 (if (>= i 0)
	     (progn 
	       (setf ls (tl-split '(#\,) line))
	       (setf s1  (string-trim '(#\space) (elt ls 0))) ;remove left and right spaces
	       ;(print s1)
	       ;(print dt)
	       (if (equal s1 dt)
		   (progn
		     (setf b1 t)
		     (return-from find-entry-dt b1)))
	     ))
	 (setf i (+ i 1)))
      (close stream)
      (return-from find-entry-dt b1)))))
      
(defun getd-cmp (fs1 fs2)               ;search stuff fs1 in fs2 then save if not found
  (let ((s1 nil)(s2 nil) (ls nil)(i 0)(b1 nil))
   (with-open-file (stream fs1
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
      (setf s2 (concatenate 'string *tmp-path* (string-trim "\"" (random-string 32)) ".csv"))
      (with-open-file (buf s2 :direction :output :if-exists :supersede)
      ;; read lines
      (when stream
      (loop for line = (read-line stream nil)
       while line do
	 (if (>= i 0)
	     (progn 
	       (setf ls (tl-split '(#\,) line))
	       (setf s1  (string-trim '(#\space) (elt ls 0)))
	       ;(print s1)
	       (setf b1 (find-entry-dt s1 fs2))
               (if (equal b1 nil) (write-line line buf))
	       ))
	 (setf i (+ i 1)))
    (close stream))
    (close buf)))
   (return-from getd-cmp s2)))	  

(defun getd-daily (fs d-prec)    ;fs is a file,  comma separated water or energy - daily sum data - d-prec is precison decimal places
  (let ((is-first nil)(cur-d nil)(new-d nil)(s1 nil) (s2 nil)(s3 nil)(ls nil)(ls1 nil)(i 0)(m1 0.0)(m2 0.0))
   (with-open-file (stream fs
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
     (setf s2 (concatenate 'string *tmp-path* (string-trim "\"" (random-string 32)) ".csv"))
     (setf is-first t)
      (with-open-file (buf s2 :direction :output :if-exists :supersede)
      ;; read lines
      (when stream
      (loop for line = (read-line stream nil)
       while line do
	 (if (>= i 0)   ;keep line one
	     (progn 
	       (setf ls (tl-split '(#\,) line))
	       (setf ls1 (tl-split '(#\space) (elt ls 0)))
	       (setf new-d (elt ls1 0))
	       
	       ;(print new-d) att  (format nil "~v$" ix px) ix is precison example 3 and px is the number
	       ;(setf s3 (string-trim '(#\space) (elt ls 2)))
	       (setf s3 (string-trim '(#\space) (elt ls 1)))
	       (if (> (length s3) 0)   ; (string-trim '(#\space) (elt ls 2))
		   (progn
		     (setf m2 (read-from-string  s3)))
		     ;(setf m2  (read-from-string (elt ls 2))))
		   (progn
		     (setf m2 0.0)))
               (if (equal is-first t)
		   (progn
		     (setf cur-d new-d)
		     (setf is-first nil)))
	       (if (equal cur-d new-d)
		   (progn
		     (setf m1 (+ m1 m2)))
		   (progn
		     (setf s1 (concatenate 'string cur-d ","  (format nil "~v$" d-prec m1)))
		     (write-line s1 buf)
		     (setf cur-d new-d)
		     (setf m1 m2)))
		     
	       ))
	 (setf i (+ i 1)))
      (close stream)
      (setf s1 (concatenate 'string new-d "," (format nil "~v$" d-prec m1) ))
      (write-line s1 buf))
    (close buf)))
   (return-from getd-daily s2)))	  

(defun getd-zero-dd (fs)  ;fs is a file,  comma separated water ror energy-  data 2  coloumns
  (let ((s1 nil) (s2 nil) (ls nil)(i 0)(b1 nil))
   (with-open-file (stream fs
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
      (setf s2 (concatenate 'string *tmp-path* (string-trim "\"" (random-string 32)) ".csv"))
      (with-open-file (buf s2 :direction :output :if-exists :supersede)
      ;; read lines
      (when stream
      (loop for line = (read-line stream nil)
       while line do
	 (if (>= i 0)    ;att la premiere ligne est déja enlevée 
	     (progn 
	       (setf ls (tl-split '(#\,) line))
	       (setf b1 nil)
		     (if (>= (length ls) 2)
			(progn
			  (if (equal (length (string-trim '(#\space) (elt ls 1))) 0)
			      (setf b1 t)
			      (progn
			      (if (= (read-from-string (elt ls 1)) 0.0) (setf b1 t))))
			  ))	       
	       (if (equal b1 t)   
		   (progn
		     (setf s1 (concatenate 'string (elt ls 0) ",0.0"))
		     (write-line s1 buf)
		     ))
	       ))
	 (setf i (+ i 1)))
    (close stream))
    (close buf)))
   (return-from getd-zero-dd s2)))

(defun getd-nrec-dt (fs dt1 dt2 f &optional (nd 7))
  (let ((s2 nil) (ls nil)(i 0)(u1 0)(u2 0)(u3 0)(ux 0))
   (with-open-file (stream fs
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
     (setf s2 (concatenate 'string *tmp-path* (string-trim "\"" (random-string 32)) ".csv"))
     (setf u1 (make-universal-time dt1 f))
     (setf u2 (make-universal-time dt2 f))
     (if (< u2 u1)
	 (progn
	   (setf ux u1)
	   (setf u1 u2)
	   (setf u2 ux)))
      (with-open-file (buf s2 :direction :output :if-exists :supersede)
      ;; read lines
      (when stream
      (loop for line = (read-line stream nil)
       while line do
	 (if (>= i 0)    ;att la premiere ligne est déja enlevée 
	     (progn 
	       (setf ls (tl-split '(#\,) line))
               (setf u3 (make-universal-time (elt ls 0) f))
	       (if  (and (>= u3 u1) (<= u3 u2))
		    (progn
		      (if (= nd 7) (write-line line buf)
			  (progn
			    (if (equal nd (day-of-week (get-day (elt ls 0) f) (get-month (elt ls 0) f) (get-year (elt ls 0) f)))
				(write-line line buf))
			    ))
		      ))
		      
	       ))
	 (setf i (+ i 1)))
    (close stream))
    (close buf)))
   (return-from getd-nrec-dt s2)))

(defun getd-all (fs)      ;fs is raw file csv extraction made with ms excel or lo calc
  (let ((s1 nil) (s2 nil) (ls nil)(ls1 nil)(ls2 nil)(i 0)(z1 nil)(z2 nil))
   (with-open-file (stream fs
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
      (setf s2 (concatenate 'string *tmp-path* (string-trim "\"" (random-string 32)) ".csv"))
      (with-open-file (buf s2 :direction :output :if-exists :supersede)
      ;; read lines
      (when stream
      (loop for line = (read-line stream nil)
       while line do
	 (if (> i 0)    ;att la premiere ligne contient le titre des colonnes 
	     (progn 
	       (setf ls (tl-split '(#\,) line))
	       (setf ls1 (tl-split '(#\space) (elt ls 0)))
	       (setf ls2 (tl-split '(#\:) (elt ls1 1)))
	       (setf z1 (elt ls2 0))
	       (setf z2 (elt ls 2))
	       (if (>= (length ls) 3)
		   (progn
		     (if (equal (length (string-trim '(#\space) z2)) 0)
			 (setf z2 "0.0")
			 (progn
			  (if (equal (read-from-string z2) 0) (setf z2 "0.0"))))
		     ))
	       (if (equal (length z1) 1)
		   (progn
		     (setf z1 (concatenate 'string "0" z1))
		     ))
	       (setf s1 (concatenate 'string (elt ls1 0) " " z1 ":" (elt ls2 1) "," (string-trim '(#\space) z2)))
	       (write-line s1 buf)
	 ))
	(setf i (+ i 1)))
    (close stream))
    (close buf)))
   (return-from getd-all s2)))  

(defun getd-3c (fs1 xs)                        ;fs1 2 columns file with lin1 title 3rd colunn created if anomalie found in xs=list of dates with conso
  (let ((s1 nil) (s2 nil)(s3 nil) (ls nil)(ls1 nil)(i 0))
   (with-open-file (stream fs1
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
      (setf s2 (concatenate 'string *tmp-path* (string-trim "\"" (random-string 32)) ".csv"))
      (with-open-file (buf s2 :direction :output :if-exists :supersede)
      ;; read lines
      (when stream
      (loop for line = (read-line stream nil)
       while line do
	 (if (>= i 0)    ;ok since 1st line
	     (progn 
	       (setf ls (tl-split '(#\,) line))
               (setf s3 nil)                  ;might become (elt ls 0) 
	       (dolist (xn (reverse xs))
		  (setf ls1 (tl-split '(#\,) xn))
                    (when (equal (elt ls 0) (elt ls1 0))
                      (setf s3 (elt ls 1))
                      (return)))
	       (setf s1 (concatenate 'string (elt ls 0) "," (elt ls 1) "," s3))
	       (write-line s1 buf)
	 ))
	(setf i (+ i 1)))
    (close stream))
    (close buf)))
   (return-from getd-3c s2)))  

(defun scan-anomz-dd (fs1 fs2 tmp tag1 tag2)   ;fs1 fs2 dd files built by us, no 1st line 2 columns tag is topic such as water or energy
                                               ;fs1 zero cons example water then tag1 is water + fs2 is dd energy + tag2 is energy 
  (let ((fz nil)(fd nil)(fe nil)(i 0)(s1 nil) (ls nil)(ls1 nil)(xs nil)
	(xs-todo nil)(xs-done nil)(b1 nil)(p 0)(wp nil) (wp1 nil) (ws nil))
    (setf fz (getd-zero-dd fs1))
       (with-open-file (stream fz
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
	 (when stream
            (loop for line = (read-line stream nil)
              while line do
	      (if (>= i 0)    ;1st line ok
	      (progn 
		(setf ls (tl-split '(#\,) line))
		(setf ls1 (tl-split '(#\space) (elt ls 0)))
		(push (elt ls1 0) xs)
		(push (get-mfirst (elt ls1 0) *md-date-only*) xs-todo)
	     ))
	  (setf i (+ i 1)))
        (close stream)))
       (run-proc-v1 6 fz 0)
       (sleep 1)
       (setf s1 (concatenate 'string tag2  "  +   zero-" tag1)) 
       (setf wp "150,150")
       (setf ws "600,400")
       (setf p 0)
       (dolist (x1 xs-todo)
          (setf b1 nil)
	  (dolist (x2 xs-done)
             (when (equal x2 x1)
                 (setf b1 t)
             (return)))
          (if (equal b1 nil)
	      (progn
		;get-mlast getd-nrec période push xs-done add 3rd col
                (setf fd (getd-nrec-dt fs2 x1 (get-mlast x1 *md-date-only*) *md-date-only*))
					;(run-proc-v1 6 fd 0)
		(setf fe (getd-3c fd xs))
                ;if show gedit is 0 then show 1 dd file fs2 + move position rotate
                (setf p (+ p 1))
		(setf wp1 (get-pos-rt ws wp p))
		(show-plot-2d1f tmp fe 0 ws wp1 s1 "time" tag2)
		(if (equal p 4) (setf p 0))
                ;(run-proc-v1 6 fs2 0) ou attendre next node to show that
		;(show-plot-2d1f tmp fe 1 "600,400" "200,200" s1 "time" tag2) showgedit 1 then display each data file
		(push x1 xs-done)
		)))
    ))

(defun getd-weekly (fs)                        ;fs data by day 2 columns no title then make  week file 1 + 7 cols
  (let ((s1 nil) (s2 nil)(ls nil)(p 0)(i 0))
   (with-open-file (stream fs
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
      (setf s2 (concatenate 'string *tmp-path* (string-trim "\"" (random-string 32)) ".csv"))
      (with-open-file (buf s2 :direction :output :if-exists :supersede)
      ;; read lines
      (when stream
      (loop for line = (read-line stream nil)
       while line do
	 (if (>= i 0)    ;ok since 1st line
	     (progn 
	       (setf ls (tl-split '(#\,) line))
               (setf p (+ p 1))
               (if (= p 1)
	           (setf s1 (concatenate 'string (elt ls 0) "," (elt ls 1) ",")))
	       (if (and (>= p 2) (<= p 7))
		   (setf s1 (concatenate 'string s1 (elt ls 1) ","))) 
	      
	       (if (= p 7)
                   (progn
		   (write-line s1 buf)
		   (setf s1 nil)
		   (setf p 0)))
	 ))
	(setf i (+ i 1)))
     (close stream))
    (if (> p 0) (write-line s1 buf)) 
    (close buf)))
   (return-from getd-weekly s2)))  

(defun init-ew (flag we  &optional (fs "*"))  ;flag gedit and file path when required
  (let ((xa nil)(wf nil)(s1 nil)(ls nil)(ps nil))
    (if (equal flag "energy-raw")
	(progn
	  (setf xa (concatenate 'string flag "=" fs))
	  (setf wf fs)
	  (push (string-trim "\"" xa) *action-flags*)))
    (if (equal flag "water-raw")
	(progn
	  (setf xa (concatenate 'string flag "=" fs))
	  (setf wf fs)
	  (push (string-trim "\"" xa) *action-flags*)))
    (if (equal flag "water-1-hour")
	(progn
	  (dolist (af *action-flags*)
	    (setf ls (tl-split '(#\=) af))
	    (when (equal (elt ls 0) "water-raw")
	      (setf s1 (elt ls 1))
	      (setf wf (getd-all s1))
	      (setf xa (concatenate 'string flag "=" wf))
	      (push (string-trim "\"" xa) *action-flags*)
	      (return)))))
    (if (equal flag "energy-15-minutes")
	(progn
	  (dolist (af *action-flags*)
	    (setf ls (tl-split '(#\=) af))
	    (when (equal (elt ls 0) "energy-raw")
	      (setf s1 (elt ls 1))
	      (setf wf (getd-all s1))
	      (setf xa (concatenate 'string flag "=" wf))
	      (push (string-trim "\"" xa) *action-flags*)
	      (return)))))
    (if (equal flag "water-per-day")
	(progn
	  (dolist (af *action-flags*)
	    (setf ls (tl-split '(#\=) af))
	    (when (equal (elt ls 0) "water-1-hour")
	      (setf s1 (elt ls 1))
	      (setf wf (getd-daily s1 2))
	      (setf xa (concatenate 'string flag "=" wf))
	      (push (string-trim "\"" xa) *action-flags*)
	      (return)))))
    (if (equal flag "energy-per-day")
	(progn
	  (dolist (af *action-flags*)
	    (setf ls (tl-split '(#\=) af))
	    (when (equal (elt ls 0) "energy-15-minutes")
	      (setf s1 (elt ls 1))
	      (setf wf (getd-daily s1 3))
	      (setf xa (concatenate 'string flag "=" wf))
	      (push (string-trim "\"" xa) *action-flags*)
	      (return)))))
    (if (equal flag "water-per-week")
	(progn
	  (dolist (af *action-flags*)
	    (setf ls (tl-split '(#\=) af))
	    (when (equal (elt ls 0) "water-per-day")
	      (setf s1 (elt ls 1))
	      (setf wf (getd-weekly s1))
	      (setf xa (concatenate 'string flag "=" wf))
	      (push (string-trim "\"" xa) *action-flags*)
	      (return)))))
    (if (equal flag "energy-per-week")
	(progn
	  (dolist (af *action-flags*)
	    (setf ls (tl-split '(#\=) af))
	    (when (equal (elt ls 0) "energy-per-day")
	      (setf s1 (elt ls 1))
	      (setf wf (getd-weekly s1))
	      (setf xa (concatenate 'string flag "=" wf))
	      (push (string-trim "\"" xa) *action-flags*)
	      (return)))))
    (if (= we 1)
	(progn
	  (setf ps (run-proc-v1 6 wf 0))
	  (sleep 2)
	  (kill-this-proc ps)
	  ))	
 ))


