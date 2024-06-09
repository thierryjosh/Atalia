;;;;
;;;; abstract layer
;;;; Resource Description Framework
;;;;
(defun make-ncbi-schema (schema name)  ;schema : schema_name    name ; db name to extract classes, properties, resources
  (let ((rq01 nil)(xml01 nil)(xml02 nil)(lsx nil)(lsy nil))
    ;;;; create rdf file
    (with-open-file (stream (concatenate 'string *rdf-path* (string-trim "\"" name) ".rdf") :direction :output :if-exists :supersede)
       (write-line (concatenate 'string "# Resource Description Framework (rdf) - schema : " schema) stream)
       (write-line "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> . " stream)
       (write-line "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> . " stream)
       (write-line "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> . " stream)
       (write-line "@prefix owl: <http://www.w3.org/2002/07/owl#> . " stream)
       ;;;;
       ;;;; get db xml description
       (check-ncbi-key)
       (build-eparams "db" name)
       (setf rq01 (call-eutils "einfo"))
       (setf xml01 (get-xmltree rq01))
       (setf xml02 (get-xml-node "FieldList" (get-xml-node "DbInfo" xml01)))
       (setf lsx (get-xml-children-nodes xml02))
       ;(princ (length lsx))
       (dolist (w lsx)
	 (setf lsy (get-xml-children-nodes w))
	 (dolist (z lsy)
	   (princ (get-tag-node z))
	   (princ "   ")
	   (princ (get-xmlstring z))
	   (terpri)
           ;(princ z)
	   ;(princ (type-of z))
	   ;(return)
	   )
	 (return); a supprimer
	 )
       (terpri stream)
       (close stream)
      )
    ))

(defun make-ncbi-db2rdf ()

  )

