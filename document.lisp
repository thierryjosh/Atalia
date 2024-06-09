;;;;
;;;;  Document : classes, methods and functions
;;;;  http://edicl.github.io/cl-ppcre/
;;;;
(defclass document()
  ((data-cluster :initarg :data-cluster
		 :initform (error "Must supply a data cluster name.")
		 :accessor data-cluster
		 :documentation "data-cluster is a list")
   (title :initarg :title
	  :initform (error "Must supply a title.")
	  :accessor title)
   (author :initarg :author
	   :initform nil
	   :accessor author
	   :documentation "author is a list")   
   (language :initarg :language
	     :initform (error "Must supply a language.")
	     :accessor language)
   (abstract :initarg :abstract
	     :initform nil
	     :accessor abstract)
   (keywords :initarg :keywords
	     :initform nil
	     :accessor keywords
	     :documentation "keywords is a list")
   (stats :initarg :stats
	  :initform nil
	  :accessor stats
	  :documentation "list of words with number of occurrences")
   (patterns :initarg :patterns
	     :initform nil
	     :accessor patterns
	     :documentation "patterns is a list")
   (publisher :initarg :publisher
	      :initform nil
	      :accessor publisher)
   (date-published :initarg :date-published
		   :initform (get-universal-time)
	           :accessor date-published
		   :documentation "number of seconds that have elapsed since 00:00 of January 1, 1900 in the GMT time zone")
   (content-reader :initarg :content-reader
		   :initform nil
	           :accessor content-reader)                   ;example : PDF
   (uri :initarg :uri
	:initform nil
	:accessor uri
	:documentation "Uniform Resource Identifier")
   (code :initarg :code
	 :initform nil
	 :accessor code)
   (code-type :initarg :code-type
	      :initform nil
	      :accessor code-type)             ;example : ISBN
   (citations :initarg :citations
	      :initform nil
	      :accessor citations
	      :documentation "citations is a list")
   (version :initarg :version
	      :initform nil
	      :accessor version
	      :documentation "list of version numbers (major minor ...) | nil  if not applicable")
   (misc :initarg :misc
	      :initform nil
	      :accessor misc
	      :documentation "list of cons  | nil  if not applicable   example (word-count . 543) ")
   ))
;Il existe une classe document puis une suite de méthode de type add pour ajouter les éléments constitutifs
;lors de la création d'un nouvel objet.
;Il y a encore d'autres méthodes pour gérer les slots, effectuer une recherche, visualiser un document.
;Les fonctions pour créer un document sont make-smple-doc  avec le minimum de renseignements ou 
;make-full-newdoc avec tous les éléments.
;La recherche est globale (search-doc) ou sur un élément de la classe (search-slotlist-doc)
;build-gen-doc permet de créer le cadac sous forme d'un string à partir d'un objet document.
;create-nn-doc permet de créer un noeud dans le graphe à partir de l'objet document.
;get-document permet de préparer le document en fonction de son type afin de récupérer un texte exploitable. 
;Les fonctions de scan permettent d'extraire et de déduire.
;Pour des raisons de performance, il existe 2 types d'analyse (simple et étendue)
;soit scan-simple-doc et scan-ext-doc.
;reset-nn-doc permet de mettre à jour le noeud du graphe en fonction de l'objet document
;remove-nn-doc supprime le noeud et l'objet
;mod-nn-if-changed permet de modifier le noeud si l'objet a changé
;					;
;exemples
;(defvar dc01 nil)
;(setf dc01 (make-simple-newdoc "covid19" "Coronavirus in Wuhan" "en")) 
;(type-of dc01)   //--> #<DOCUMENT {1005D0DFE3}>
;(data-cluster dc01)     //-->("covid19")
;(add-keywords-doc dc01 "China")   //-->("China")
;(add-patterns-doc dc01 "Clinical features of patients infected with 2019 novel coronavirus in Wuhan")
;//-->("Clinical features of patients infected with 2019 novel coronavirus in Wuhan")
;(describe dc01)
;(search-doc dc01 "wuhan")
;//-->(("title" "Coronavirus in Wuhan" "wuhan" 0.25)
; ("patterns"
;  "Clinical features of patients infected with 2019 novel coronavirus in Wuhan"
;  "wuhan" 0.06666667))
;
;(scan-title2-doc "/home/arkho/.emacs.d/document/A new coronavirus associated with human respiratory disease in China.pdf")
;//-->"A new coronavirus associated with human respiratory disease in China"
;"pdf"
;(defvar dc02 nil)
;(setf dc02 (scan-ext-doc "/home/arkho/.emacs.d/document/A new coronavirus associated with human respiratory disease in China.pdf" "*" "en"))
;(describe dc02)
;
(defmethod add-data-cluster-doc ((obj document) dc)
  (if (equal (typep dc 'cons) T)
      (setf (data-cluster obj) dc)
      (push dc (data-cluster obj)))
  )

(defmethod add-author-doc ((obj document) au)
  (if (equal (typep au 'cons) T)
      (setf (author obj) au)
      (push au (author obj)))
  )

(defmethod add-abstract-doc ((obj document) ab)
   (setf (abstract obj) ab)
  )

(defmethod add-title-doc ((obj document) tt)
   (setf (title obj) tt)
  )

(defmethod add-language-doc ((obj document) lg)
   (setf (language obj) (make-uppercase lg))
  )

(defmethod add-keywords-doc ((obj document) kw)
  (if (equal (typep kw 'cons) T)
      (setf (keywords obj) kw)
      (push kw (keywords obj)))
  )

(defun make-stats-doc (wd nb)
   (return-from make-stats-doc (cons wd nb)) 
  )

(defmethod add-stats-doc ((obj document) st)
  (if (equal (typep st 'cons) T)
      (push st (stats obj)))
  )

(defmethod add-patterns-doc ((obj document) pt)
  (if (equal (typep pt 'cons) T)
      (setf (patterns obj) pt)
      (push pt (patterns obj)))
  )

(defmethod add-publisher-doc ((obj document) pb)
   (setf (publisher obj) pb)
  )

(defmethod add-date-published-doc ((obj document) dp)
  (if (equal (typep dp 'integer) T)
   (setf (date-published obj) dp))
  )

(defmethod add-content-reader-doc ((obj document) cr)
   (setf (content-reader obj) (make-uppercase cr))
  )

(defmethod add-uri-doc ((obj document) ur)
   (setf (uri obj) ur)
  )

(defmethod add-code-doc ((obj document) cd)
   (setf (code obj) cd)
  )

(defmethod add-code-type-doc ((obj document) ct)
   (setf (code-type obj) (make-uppercase ct))
  )

(defmethod add-citations-doc ((obj document) ci)
  (if (equal (typep ci 'cons) T)
      (setf (citations obj) ci)
      (push ci (citations obj)))
  )

(defmethod add-version-doc ((obj document) vs)
  (if (equal (typep vs 'cons) T)
      (setf (version obj) vs)
      (push vs (version obj)))
  (setf (version obj) (reverse (version obj)))
  )

(defun make-misc-doc (p  nb)
   (return-from make-misc-doc (cons p nb)) 
  )

(defmethod add-misc-doc ((obj document) mc)
  (if (equal (typep mc 'cons) T)
      (push mc (misc obj)))
  )

(defmethod erase-slot-doc ((obj document) slot-accessor)
  (setf (slot-value obj (read-from-string slot-accessor)) nil)
 )

(defmethod remove-slotlist-doc ((obj document) slot-accessor item)  ;remove an item from the list (string or cons)
  (let ((w nil))
    (if (equal (typep (slot-value obj (read-from-string slot-accessor)) 'cons) T)
	(progn
            (dolist (z (slot-value obj (read-from-string slot-accessor)))
               (setf w z)
               (if (equal (typep w 'cons) T) (setf w (car z)))
	       (when (equal w item)
                  (setf (slot-value obj (read-from-string slot-accessor)) (remove z (slot-value obj (read-from-string slot-accessor))))
                  )))
	(progn
	  (setf (slot-value obj (read-from-string slot-accessor)) nil)))
      (return-from remove-slotlist-doc t)	  
    ) 
  )

;mettre strict en option pour recherche avec defaul not strict
;2 types de recgherche  a) tout le document search-doc et b) un slot désigné

(defmethod search-slotlist-doc ((obj document) slot-accessor item) ;search an item(string)  in a string or in a  list made of (string or cons)
  (let ((w nil)(w1 nil)(rs nil)(ls nil))
    (if (equal (typep (slot-value obj (read-from-string slot-accessor)) 'cons) T)
	(progn
          (dolist (z (slot-value obj (read-from-string slot-accessor)))
            (setf w z)
            (if (equal (typep w 'cons) T) (setf w (car z)))
	    (setf w1 w)
            (setf w (make-lowercase w))
	    (when (not (equal (search (make-lowercase item) w) nil))
              (push slot-accessor ls)
	      (push w1 ls)
	      (push item  ls)
	      (push (float (/ (length item) (length w))) ls)
	      (push (reverse ls) rs)
	      (setf ls nil))))
	 (progn
           (setf w  (slot-value obj (read-from-string slot-accessor)))
	   (setf w1 w)
	   (setf w (make-lowercase w))
	   (if (not (equal (search (make-lowercase item) w) nil))
	       (progn
                 (push slot-accessor ls)
	         (push w1 ls)
	         (push item  ls)
	         (push (float (/ (length item) (length w))) ls)
	         (push (reverse ls) rs)
		 ))
	  ))    
    (return-from search-slotlist-doc rs) 
    )
  )

(defmethod search-doc ((obj document) item)
  (let ((rs nil)(z nil))
    (setf z (search-slotlist-doc obj "data-cluster" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "title" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "author" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "language" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "abstract" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "keywords" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "stats" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "patterns" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "publisher" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "date-published" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "content-reader" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "uri" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "code" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "code-type" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "citations" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "version" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (setf z (search-slotlist-doc obj "misc" item))
    (if (not (equal z nil)) (setf rs (concatenate 'list rs z)))
    (return-from search-doc rs)
    )
  )

(defmethod show-doc ((obj document))
  (if (AND (equal (content-reader obj) "PDF") (not (equal (uri obj) nil)))
      (run-proc-v1 8 (uri obj) 0))
  )

(defun make-simple-newdoc (dc tt lg)
  (let ((docobj nil))
    (if (equal (typep dc 'cons) T)
        (setf docobj (make-instance 'document :data-cluster dc :title tt :language (make-uppercase lg)))
	(setf docobj (make-instance 'document :data-cluster (list dc) :title tt :language (make-uppercase lg))))
    (return-from make-simple-newdoc docobj)
    )
  )

(defun make-full-newdoc (dc tt au lg ab kw st pt pb dp cr ur cd ct ci vs mc)
  (let ((docobj nil))
    (setf docobj (make-simple-newdoc dc tt lg))
    (if (not (equal au nil)) (add-author-doc docobj au))
    (add-abstract-doc docobj ab)
    (if (not (equal kw nil))(add-keywords-doc docobj kw))
    (if (not (equal st nil))(add-stats-doc docobj st))
    (if (not (equal pt nil))(add-patterns-doc docobj pt))
    (add-publisher-doc docobj pb)
    (add-date-published-doc docobj dp)
    (add-content-reader-doc docobj cr)
    (add-uri-doc docobj ur)
    (add-code-doc docobj cd)
    (add-code-type-doc docobj ct)
    (if (not (equal ci nil))(add-citations-doc docobj ci))
    (if (not (equal vs nil)) (add-version-doc docobj vs))
    (if (not (equal mc nil)) (add-misc-doc docobj mc))
    (return-from make-full-newdoc docobj)
    )
  )

(defmethod build-gen-doc ((obj document))
  (let ((s1 nil)(s2 nil)(s3 nil)(s4 nil)(s5 nil)(s6 nil)(au nil)(dc nil)(tt nil)(lg nil)(ab nil)(kw nil)(pt nil)
	(cd nil)(cr nil)(ur nil)(ci nil)(vs nil)(mc nil)(ct nil)(dp nil)(pb nil)(st nil)(stream  (make-string-output-stream)))
    (write-line ";;;;Document instance" stream)
    (setf dc (data-cluster obj))
    (setf s1 (enum-string-list dc))
    (setf tt (title obj))
    (setf lg (language obj))  
    (write-line (concatenate 'string "(setf *docobj* (make-simple-newdoc (list " s1  ") §" tt "§ " "§" lg "§))") stream)
    (setf au (author obj))
    (if (not (equal au nil))
      (progn					
        (setf s2 (enum-string-list au))
        (write-line (concatenate 'string "(add-author-doc *docobj* (list " s2 "))") stream)
      ))
    (setf ab (abstract obj))
    (if (not (equal ab nil))(write-line (concatenate 'string "(add-abstract-doc *docobj* §" ab "§)") stream))
    (setf kw (keywords obj))
    (if (not (equal kw nil))
      (progn					
        (setf s3 (enum-string-list kw))
        (write-line (concatenate 'string "(add-keywords-doc *docobj* (list " s3 "))") stream)
	))
    (setf st (stats obj))
    (if (not (equal st nil))
      (progn
        (dolist (z1 (reverse st))
          (write-line (concatenate 'string "(add-stats-doc *docobj* (make-stats-doc §" (car z1) "§ " (write-to-string (cdr z1)) "))") stream)
	  )
	))
    (setf pt (patterns obj))
    (if (not (equal pt nil))
      (progn					
        (setf s4 (enum-string-list pt))
        (write-line (concatenate 'string "(add-patterns-doc *docobj* (list " s4 "))") stream)
	))
    (setf pb (publisher obj))
    (if (not (equal pb nil))(write-line (concatenate 'string "(add-publisher-doc *docobj* §" pb "§)") stream))
    (setf dp (date-published obj))
    (if (not (equal dp nil))(write-line (concatenate 'string "(add-date-published-doc *docobj* " (write-to-string dp)  ")") stream))
    (setf cr (content-reader obj))
    (if (not (equal cr nil))(write-line (concatenate 'string "(add-content-reader-doc *docobj* §" cr "§)") stream))
    (setf ur (uri obj))
    (if (not (equal ur nil))(write-line (concatenate 'string "(add-uri-doc *docobj* §" ur "§)") stream))
    (setf cd (code obj))
    (if (not (equal cd nil))(write-line (concatenate 'string "(add-code-doc *docobj* §" cd "§)") stream))
    (setf ct (code-type obj))
    (if (not (equal ct nil))(write-line (concatenate 'string "(add-code-type-doc *docobj* §" ct "§)") stream))
    (setf ci (citations obj))
    (if (not (equal ci nil))
      (progn					
        (setf s5 (enum-string-list ci))
        (write-line (concatenate 'string "(add-citations-doc *docobj* (list " s5 "))") stream)
	))
    (setf vs (version obj))
    (if (not (equal vs nil))
      (progn					
        (setf s6 (enum-string-list (reverse vs)))    
        (write-line (concatenate 'string "(add-version-doc *docobj* (list " s6 "))") stream)
	))
    (setf mc (misc obj))
    (if (not (equal mc nil))
      (progn
        (dolist (z1 (reverse mc))
          (write-line (concatenate 'string "(add-misc-doc *docobj* (make-misc-doc §" (car z1) "§ " (write-to-string (cdr z1)) "))") stream)
	  )
	))    
    (return-from build-gen-doc (get-output-stream-string stream))
    )
  )

(defmethod create-nn-doc ((obj document)) ;attention au nombre d'espaces entre les mots du titre en préparant le document
  (let ((seed-cadac nil)(dc nil)(tt nil)(lg nil)(ssx nil))
    (setf tt (title obj))
    (if (equal tt nil) (return-from create-nn-doc nil))
    (setf dc (data-cluster obj))
    (if (equal dc nil)(return-from create-nn-doc nil))
    (setf lg (language obj))
    (if (equal lg nil)(return-from create-nn-doc nil))
    (if (equal (create-node tt) nil)(return-from create-nn-doc nil)) 
    (setf seed-cadac (mk-cadac-node tt))
    (if (equal seed-cadac nil) (return-from create-nn-doc nil))
    (if (equal *flag-seed-doc* nil)
	(progn
          (if (equal (find-seed "document") nil)
	       (create-seed "document" 1 1 -1))
	  (setf *flag-seed-doc* T)  
	  ))
    (add-seed-to-node "document" tt)
    (setf ssx (build-gen-doc obj))
    (update3-desc-seed seed-cadac ssx)
    )
  )

(defun get-document (fdoc docv)  
  (if (equal docv 1)    ;PDF
      (return-from get-document (pdf2text fdoc 2))) 
  )

; also to consider (pathname-name (pathname "/home/arkho/.emacs.d/atalia-pr3-en.pdf")) ==> atalia-pr3-en
;(pathname-type (pathname "/home/arkho/.emacs.d/atalia-pr3-en.pdf")) ==> pdf
(defun scan-title-doc (fdoc doctype)
  (let ((lw nil)(lt nil)(i 0)(s1 nil))
    (setf lw  (ppcre:all-matches-as-strings "[\\w\\s_-]+" fdoc))
    (if (and (not (equal lw nil)) (>= (length lw) 2))
	(progn
          (setf i (length lw))
	  (if (equal (make-lowercase doctype) (make-lowercase (elt lw (- i 1))))
	      (progn
                (setf lt (ppcre:all-matches-as-strings "[a-zA-Z0-9]+" (elt lw (- i 2))))
		(dolist (z lt)
                  (setf s1 (concatenate 'string s1 z '(#\space)))		  
		  )
		;(if (not (equal s1 nil))(setf s1 (subseq s1 0 (- (length s1) 1))))
		(setf s1 (string-trim '(#\space) s1))
		(return-from scan-title-doc s1)
		))
	  ))
    (return-from scan-title-doc nil)
    ))

(defun scan-title2-doc (fdoc)
  (let ((lw nil)(doctype nil)(s1 nil)(docname nil))
    (setf doctype (pathname-type (pathname fdoc)))
    (setf docname (pathname-name (pathname fdoc)))
    (setf lw (ppcre:all-matches-as-strings "[a-zA-Z0-9]+" docname))
    (if (not (equal lw nil))
	(progn
          (dolist (z lw) (setf s1 (concatenate 'string s1 z '(#\space))))
	  (setf docname (string-trim '(#\space) s1))	  
	  ))
    (return-from scan-title2-doc (values docname doctype))
    )
  )

(defun cmp-lst-cons (a b)
   (return-from cmp-lst-cons (> (cdr a) (cdr b))) 
  )

(defun sort-lst-cons (ls)
  (return-from sort-lst-cons  (stable-sort ls 'cmp-lst-cons))
  )

(declaim (ftype function scan-lexicon))

;table of contents 1
;first  element is numeric (l<=3), Exits Noun or verb, tot length less than a line
;or first  element is uppercase(l<=3), Exits Noun or verb, tot length less than a line

(defun scan-toc1-doc (line ls)
  (let ((ld nil)(s1 nil)(k 0)(b nil))
    (if (or (equal (length line) 0)(equal ls nil))(return-from scan-toc1-doc nil))
    (setf ld (ppcre:all-matches-as-strings "\\d+" line))
    
    (if (not (equal ld nil))
	(if (equal (elt ld 0)(elt ls 0))(setf b T)))
    (if (and (equal b nil) (equal (elt ls 0) (make-uppercase (elt ls 0))))(setf b T))
    
    (if (and (<= (length line) *flag-line*) (<= (length (elt ls 0)) 3))
        (if (equal b T)
	    (progn
              (dolist (z ls)
                (multiple-value-bind (w1 p1 d1) (scan-lexicon z)
		  (declare (ignore w1 d1)) ;(print z) (print p1)
                  (if (or (equal p1 "Verb")(equal p1 "Noun"))(incf k))
		      
		  (if (equal s1 nil)
		      (if (or (equal p1 "Verb")(equal p1 "Noun"))(setf s1 (concatenate 'string s1 z " ")))
		      (setf s1 (concatenate 'string s1 z " ")))
		  )
		;(print s1)
		)
	      )))
    (if(and (not (equal s1 nil))(> k 0))(return-from scan-toc1-doc (string-trim '(#\space) s1))
	(return-from scan-toc1-doc nil))
    )
  )

;table of contents 2
;all line uppercase
(defun scan-toc2-doc (line ls)
  (let ((s1 nil))
    (if (or (equal (length line) 0)(equal ls nil))(return-from scan-toc2-doc nil))
    (if (and (<= (length line) *flag-line*) (> (length ls) 1) (equal line (make-uppercase line)))
	(progn
	  (setf s1 (string-trim '(#\space) line))
	  (return-from scan-toc2-doc s1))
	(return-from scan-toc2-doc nil))
    )
  )

(defun scan-ext-doc (fdoc doctype lg &optional (dc "document")(au nil))     ;PDF HTML XML TXT DOC DOCX ODT
  (let ((ftxt nil)(lw nil)(word-count 0)(tt nil)(docobj nil)(docwords nil)(b1 T)(k 0)(i 0)(sx nil)(tc nil)(nb-lines 0)(ln-lines 0))
    (if (equal (probe-file fdoc) nil)(return-from scan-ext-doc nil))
    (if (equal doctype "*")
	(progn
	  (multiple-value-bind (n1 n2) (scan-title2-doc fdoc)
	    (setf doctype (make-uppercase n2))
	    (setf tt n1)))
	(setf tt (scan-title-doc fdoc doctype)))
	  
    (if (equal doctype "PDF") (setf ftxt (get-document fdoc 1)))
    (if (equal doctype "TXT") (setf ftxt fdoc))
    
    (setf docobj (make-simple-newdoc dc tt lg))
    (if (not (equal au nil))(add-author-doc docobj au))
    (add-content-reader-doc docobj (make-uppercase doctype))
    (add-uri-doc docobj fdoc)
    (push (cons "*" 0) docwords)
     (with-open-file (stream ftxt
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
      (when stream
      (loop for line = (read-line stream nil)
	 while line do
	   (if (> (length line) 1)(incf nb-lines))
	   (setf ln-lines (+ ln-lines (length line)))
	   (setf lw  (ppcre:all-matches-as-strings "\\w+" line))
	   (setf word-count (+ word-count (length lw)))
	   (if (not (equal lw nil))
	       (progn
                 (dolist (z lw)
		   (setf b1 T)
		   (setf k -1)
                   (dolist (w docwords)
                     (when (equal (make-lowercase (car w)) (make-lowercase z))
                       (setf b1 nil)
		       (setf k (position w docwords))
                       (return))		      
		     )
		   (if (equal b1 T)(push (cons z 1) docwords)
		       (progn
			  (if (>= k 0)
                           (setf (elt docwords k) (cons (car (elt docwords k)) (+ (cdr (elt docwords k)) 1))))
			 ))
		   )
		 ))
	   )
      ;(print (length docwords))

      (add-misc-doc docobj (cons "nb-lines" nb-lines))
      (add-misc-doc docobj (cons "average-line" (float (/ ln-lines nb-lines))))
      (add-misc-doc docobj (cons "doc-length" ln-lines))
      (add-misc-doc docobj (cons "word-count" word-count))
      (setf docwords (sort-lst-cons docwords))
      (setf i 0)
      (dolist (w docwords)
        (if (> (length (car w)) 3)
	    (progn
              (setf b1 T)
	      (if (equal (make-uppercase lg) "EN")
		  (progn
                    (multiple-value-bind (w1 p1 d1) (scan-lexicon (car w))
		      (declare (ignore w1 d1))
                      (if (not (equal p1 "Noun"))(setf b1 nil))
		      )
		    ))
	      (if (equal b1 T)
		  (progn
                    (add-stats-doc docobj w)
		    (setf i (+ i 1))
		    ))
	      ))
	(if (>= i *flag-stats*)(return))
	)
      (close stream))
      ;(print (length docwords2))
      (with-open-file (stream ftxt
                     :if-does-not-exist nil
                     :external-format '(:utf-8 :replacement "?"))
         (when stream
            (loop for line = (read-line stream nil)
	       while line do
		 (setf lw  (ppcre:all-matches-as-strings "\\w+" line))
		 (if (equal (make-uppercase lg) "EN")
		     (progn
		       (setf sx nil)
                       (setf sx (scan-toc1-doc line lw))
		       (if (not (equal sx nil))(push sx tc))
		       (setf sx (scan-toc2-doc line lw))
		       (if (not (equal sx nil))(push sx tc))
		       (if (not (equal (stats docobj) nil))
			   (dolist (z (stats docobj))
				   (if (and (not (equal (search (make-lowercase (car z)) line) nil)) (>= (length line) *flag-line*))
				       (push (replace-all line "\"" "'") tc))
				       
			     ))
		       ))
	    )
	 )
      (close stream))
      (setf tc (remove-duplicate-from-list tc))
      (setf tc (remove-meaningless-from-list tc))
      (if (not (equal tc nil))
	  (progn
            (dolist (w tc)
              (setf lw  (ppcre:all-matches-as-strings "\\w+" w))
	      (if (= (length lw) 1)(add-keywords-doc docobj (elt lw 0)))
	      (if (> (length lw) 1)(add-patterns-doc docobj w))
	      )
	    ))
      (return-from scan-ext-doc docobj))  ;nust return docobj
    )
  )

(defun scan-simple-doc (fdoc doctype lg &optional (dc "document")(au nil))     ;PDF HTML XML TXT DOC DOCX ODT
  (let ((tt nil)(docobj nil))
    (if (equal (probe-file fdoc) nil)(return-from scan-simple-doc nil))
    (if (equal doctype "*")
	(progn
	  (multiple-value-bind (n1 n2) (scan-title2-doc fdoc)
	    (setf doctype (make-uppercase n2))
	    (setf tt n1)))
	(setf tt (scan-title-doc fdoc doctype)))
    (setf docobj (make-simple-newdoc dc tt lg))
    (if (not (equal au nil))(add-author-doc docobj au))
    (add-content-reader-doc docobj (make-uppercase doctype))
    (add-uri-doc docobj fdoc)
    (return-from scan-simple-doc docobj)  ;nust return docobj
    )
  )

(defmethod reset-nn-doc ((obj document))
  (let ((nd-name nil)(nd-obj nil)(sd-name nil)(ssx nil))
    (setf nd-name (title obj))
    (if (not (equal nd-name nil)) (setf nd-obj (find-node2 nd-name)))
    (if (not (equal nd-obj nil)) (setf sd-name (is-cadac2 nd-obj)))
    (setf ssx (build-gen-doc obj))
    (if (not (equal sd-name nil))(update3-desc-seed sd-name ssx))  
    )
  )

(defmethod remove-nn-doc ((obj document))
  (let ((nd-name nil)(nd-obj nil)(sd-name nil))
    (setf nd-name (title obj))
    (if (not (equal nd-name nil)) (setf nd-obj (find-node2 nd-name)))
    (if (not (equal nd-obj nil)) (setf sd-name (is-cadac2 nd-obj)))
    (if (not (equal sd-name nil)) (remove-cadac sd-name))
    (if (equal (remove-node nd-name) T)
        (progn
	  (setf (data-cluster obj) nil) ;att ramasse miette ne pas oublier de détruire l'objet après remove-nn-doc exit
	  (setf (title obj) nil)
	  (setf (author obj) nil)
	  (setf (language obj) nil)
	  (setf (abstract obj) nil)
	  (setf (keywords obj) nil)
	  (setf (stats obj) nil)
	  (setf (patterns obj) nil)
	  (setf (publisher obj) nil)
	  (setf (date-published obj) nil)
	  (setf (content-reader obj) nil)
	  (setf (uri obj) nil)
	  (setf (code obj) nil)
	  (setf (code-type obj) nil)
	  (setf (citations obj) nil)
	  (setf (version obj) nil)
	  (setf (misc obj) nil)
	  (return-from remove-nn-doc T)))   
    )
  )

(defun cmp-obj-doc (obj1 obj2)
  (let ((ms nil))
    (if (not (equal (data-cluster obj1) (data-cluster obj2)))(setf ms (concatenate 'string ms "dc")))
    (if (not (equal (title obj1) (title obj2)))(setf ms (concatenate 'string ms "tt")))
    (if (not (equal (author obj1) (author obj2)))(setf ms (concatenate 'string ms "au")))
    (if (not (equal (language obj1) (language obj2)))(setf ms (concatenate 'string ms "lg")))
    (if (not (equal (abstract obj1) (abstract obj2)))(setf ms (concatenate 'string ms "ab")))
    (if (not (equal (keywords obj1) (keywords obj2)))(setf ms (concatenate 'string ms "kw")))
    (if (not (equal (stats obj1) (stats obj2)))(setf ms (concatenate 'string ms "st")))
    (if (not (equal (patterns obj1) (patterns obj2)))(setf ms (concatenate 'string ms "pt")))
    (if (not (equal (publisher obj1) (publisher obj2)))(setf ms (concatenate 'string ms "pb")))
    (if (not (equal (date-published obj1) (date-published obj2)))(setf ms (concatenate 'string ms "dp")))
    (if (not (equal (content-reader obj1) (content-reader obj2)))(setf ms (concatenate 'string ms "cr")))
    (if (not (equal (uri obj1) (uri obj2)))(setf ms (concatenate 'string ms "ur")))
    (if (not (equal (code obj1) (code obj2)))(setf ms (concatenate 'string ms "cd")))    
    (if (not (equal (code-type obj1) (code-type obj2)))(setf ms (concatenate 'string ms "ct")))
    (if (not (equal (citations obj1) (citations obj2)))(setf ms (concatenate 'string ms "ci")))
    (if (not (equal (version obj1) (version obj2)))(setf ms (concatenate 'string ms "vs")))
    (if (not (equal (misc obj1) (misc obj2)))(setf ms (concatenate 'string ms "mc")))    
    (return-from cmp-obj-doc ms)
    )
  )

(defmethod mod-nn-if-changed ((obj document)) ;modify node if object instance of document has changed 
  (let ((nd-obj nil)(sd-name nil)(ssx nil)(ms nil))
    (if (equal (title obj) nil)(return-from mod-nn-if-changed nil)
	(setf nd-obj (find-node2 (title obj))))
    (if (not (equal nd-obj nil))
	(progn
	  (setf *tmp-obj-doc* nil)
          (run-cadac nd-obj "*tmp-obj-doc*")    ;(symbol-name 'var)
	  (setf sd-name (is-cadac2 nd-obj))
	  ))
    (if (equal *tmp-obj-doc* nil)(return-from mod-nn-if-changed nil))
    (if (equal sd-name  nil)(return-from mod-nn-if-changed nil))
    (setf ms (cmp-obj-doc *tmp-obj-doc* obj))
    (if (not (equal ms nil))
	(progn
          (setf ssx (build-gen-doc obj))
	  (update3-desc-seed sd-name ssx)
	  (return-from mod-nn-if-changed ms)
	  ))
    (return-from mod-nn-if-changed nil)
    )
  )
