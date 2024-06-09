;;;;
;;;;  office  abstract layer 
;;;;
;;;;  /usr/bin/soffice --headless --convert-to txt --outdir /home/arkho/.emacs.d/tmp /home/arkho/.emacs.d/spores-dev-notes.odt 
;;;;
;;;;  
;;;;
;;;;  
;;;;
(defun officonv (file-to-conv tg-type)
 ;30 soffice xscript=--headless§--convert-to§txt§--outdir§/home/arkho/.emacs.d/tmp§/home/arkho/.emacs.d/spores-dev-notes.odt
  (let ((fs nil)(outdir nil)(xscript nil))
      (setf outdir (subseq *tmp-path* 0 (- (length *tmp-path*) 1)))
      (setf xscript (concatenate 'string "---headless§--convert-to§" tg-type "§--outdir§" outdir "§" file-to-conv))
      (setf fs file-to-conv) 
      ;(princ xscript)
      (run-proc-v1 30 xscript 1)
      (return-from officonv fs)
    )
  )
