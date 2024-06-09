;;;;
;;;; xml abstract layer (éviter la dépendance sur une lib xml
;;;;
;;;;  https://edicl.github.io/drakma/
;;;;
;;;;  https://common-lisp.net/project/xmls/
;;;;
;;;;  (defvar *xnd* (xmls:parse *rqt*))
;;;;  (xmls:xmlrep-string-child (xmls:xmlrep-find-child-tag "WebEnv" *xnd*))
;;;;  "NCID_1_59641565_130.14.22.76_9001_1571918432_1917455095_0MetA0_S_MegaStore"
;;;;
;;;;  "xmlrep-find-child-tag" est un xmls:node alors que "xmlrep-find-child-tags" est CONS (liste de nodes)
;;;;  (type-of (nth 0 (xmls:xmlrep-find-child-tags "WebEnv" *xnd*))  "--> xmls:node
;;;;
;;;;  (xmls:xmlrep-string-child (nth 0 (xmls:xmlrep-find-child-tags "WebEnv" *xnd*)))
;;;;  "NCID_1_59641565_130.14.22.76_9001_1571918432_1917455095_0MetA0_S_MegaStore"
;;;;
;;;; returns a list a childreen (cons)
;;;; (xmls:xmlrep-children  (xmls:xmlrep-find-child-tag "IdList" (get-xmltree *qx1*)))
;;;;
(defun get-xmltree (my-xml)
  ;builds an xml tree
  (return-from get-xmltree (xmls:parse my-xml))
  )

(defun get-xml-node (tag1 xmltree)
  ;returns an xml node 
  (return-from get-xml-node (xmls:xmlrep-find-child-tag tag1 xmltree))
  )

(defun get-tag-node (xmltree)
  ;rerurns node tag
  (return-from get-tag-node (xmls:xmlrep-tag xmltree))
  )

(defun get-xml-children-nodes (xmltree)
  ;returns nodes
  (return-from get-xml-children-nodes (xmls:xmlrep-children xmltree))
  )

(defun getstr-singlenode (tag1 xmltree)
  ;returns a string matching tag1 "vector character"
  (return-from getstr-singlenode (xmls:xmlrep-string-child (xmls:xmlrep-find-child-tag tag1 xmltree)))
  )

(defun get-childrenlist (tag1 xmltree)
  ;returns cons "a list"
  (return-from get-childrenlist (xmls:xmlrep-children  (xmls:xmlrep-find-child-tag tag1 xmltree)))
  )

(defun get-xmlstring (xn)
  (return-from get-xmlstring (xmls:xmlrep-string-child xn)) 
  )

(defun getstr-firstnode (tag1 xmltree)
  ;returns a string matching tag1 "vector character"
  (return-from getstr-firstnode (xmls:xmlrep-string-child (nth 0 (xmls:xmlrep-find-child-tags tag1 xmltree))))
  )

