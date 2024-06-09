;;;;
;;;; htm/html  abstract layer 
;;;;
;;;;  curl https://www.arkho.net -o /home/arkho/.emacs.d/arkho1.html
;;;;
;;;;  html2text -o /home/arkho/.emacs.d/arkho1.txt /home/arkho/.emacs.d/arkho1.html
;;;;
;;;;  use plump parser for tags https://github.com/Shinmera/plump
;;;;
(defun html2text (htm)
  (let ((fs nil)(xscript nil))
    (setf fs  (replace-all htm ".html" ".txt" ))
    (setf xscript (concatenate 'string "-o§" fs "§" htm))   ; 20 html2text xscript=-o§outputfiletext§inputfileehtml
    (run-proc-v1 20 xscript 1)
    (return-from html2text fs)
    )
  )

(defun url2text (url)
  (let ((fs nil)(ft nil)(xscript nil))
    (setf fs (concatenate 'string *tmp-path* (string-trim "\"" (random-string 32)) ".html"))
    (setf xscript (concatenate 'string url "§-o§" fs))   ; 10 curl xscript=url§-o§randomfilehtml
    (run-proc-v1 10 xscript 1)
    (setf ft (html2text fs))
    (return-from url2text ft)
    )
  )

