;;;;
;;;;  chess-pgn : classes, methods and functions
;;;;  arena :  http://www.playwitharena.de/
;;;;  pgn : https://en.wikipedia.org/wiki/Portable_Game_Notation
;;;;  http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
;;;;  download pgn files https://www.pgnmentor.com/files.html
;;;;  https://chessopenings.com/eco/
;;;;  https://www.chessgames.com/chessecohelp.html
;;;;  https://www.365chess.com/eco.php
;;;;
(defclass portable-game-notation()
;mandatory tags - seven tag roster
  ((Event :initarg :Event
		 :initform (error "Must supply the name of the tournament or match event.")
		 :accessor Event
		 :documentation "Event is a list")
   (Site :initarg :Site
	   :initform (error "Must supply the location of the event.")
	   :accessor Site
	   :documentation "Site is a list")   
   (Date :initarg :Date
	   :initform (error "Must supply the starting date of the game.")
	   :accessor Date
	   :documentation "Date  is a list")
   (zRound :initarg :zRound       ;att Round  reserved symbol
	   :initform (error "Must supply the playing round ordinal of the game.")
	   :accessor zRound
	   :documentation "Round  is a list")
   (White :initarg :White
	   :initform (error "The player(s) of the white pieces.")
	   :accessor White
	   :documentation "White  is a list")
   (Black :initarg :Black
	   :initform (error "The player(s) of the black pieces.")
	   :accessor Black
	   :documentation "Black  is a list")
   (Result :initarg :Result
	   :initform (error "Must supply the result of the game.")
	   :accessor Result
	   :documentation "Result  is a list")
;supplemental tags 
   (WhiteTitle :initarg :WhiteTitle
	   :initform nil
	   :accessor WhiteTitle
	   :documentation "White title such as FM, IM, and GM")
   (BlackTitle :initarg :BlackTitle
	   :initform nil
	   :accessor BlackTitle
	   :documentation "Black title such as FM, IM, and GM")
   (WhiteElo :initarg :WhiteElo
	   :initform nil
	   :accessor WhiteElo
	   :documentation "White : FIDE Elo rating.")
   (BlackElo :initarg :BlackElo
	   :initform nil
	   :accessor BlackElo
	   :documentation "Black : FIDE Elo rating.")
   (WhiteUSCF :initarg :WhiteUSCF
	   :initform nil
	   :accessor WhiteUSCF
	   :documentation "White : United States Chess Federation rating.")
   (BlackUSCF :initarg :BlackUSCF
	   :initform nil
	   :accessor BlackUSCF
	   :documentation "Black :  United States Chess Federation rating.")
   (WhiteNA :initarg :WhiteNA
	   :initform nil
	   :accessor WhiteNA
	   :documentation "White : e-mail or network addresses of the player.")
   (BlackNA :initarg :BlackNA
	   :initform nil
	   :accessor BlackNA
	   :documentation "Black : e-mail or network addresses of the player.")
   (WhiteType :initarg :WhiteType
	   :initform nil
	   :accessor WhiteType
	   :documentation "White : the player type such as human, program, AI, ... ")
   (BlackType :initarg :BlackType
	   :initform nil
	   :accessor BlackType
	   :documentation "Black : the player type such as human, program, AI, ...")
   (EventDate :initarg :EventDate
	   :initform nil
	   :accessor EventDate
	   :documentation "Starting date of the event.")
   (EventSponsor :initarg :EventSponsor
	   :initform nil
	   :accessor EventSponsor
	   :documentation "The name of the sponsor of the event.")
   (Section :initarg :Section
	   :initform nil
	   :accessor Section
	   :documentation "The playing section of a tournament.")
   (Stage :initarg :Stage
	   :initform nil
	   :accessor Stage
	   :documentation "The stage of a multistage event.")
   (Board :initarg :Board
	   :initform nil
	   :accessor Board
	   :documentation "The board number.")
   (Opening :initarg :Opening
	   :initform nil
	   :accessor Opening
	   :documentation "The traditional opening name.")
   (Variation :initarg :Variation
	   :initform nil
	   :accessor Variation
	   :documentation "This is used to further refine the Opening tag.")
   (SubVariation :initarg :SubVariation
	   :initform nil
	   :accessor SubVariation
	   :documentation "This is used to further refine the Variation tag.")
   (ECO :initarg :ECO
	   :initform nil
	   :accessor ECO
	   :documentation "Opening code from the five volume Encyclopedia of Chess Openings.")
   (NIC :initarg :NIC
	   :initform nil
	   :accessor NIC
	   :documentation "Opening code from the New in Chess database.")
   (zTime :initarg :zTime       ;att Time reserved symbol
	   :initform nil
	   :accessor zTime
	   :documentation "Time-of-day value in the form HH:MM:SS")
   (UTCTime :initarg :UTCTime
	   :initform nil
	   :accessor UTCTime
	   :documentation "Time : Universal Coordinated Time standard.")
   (UTCDate :initarg :UTCDate
	   :initform nil
	   :accessor UTCDate
	   :documentation "Date : Universal Coordinated Time standard.")
   (TimeControl :initarg :TimeControl
	   :initform nil
	   :accessor TimeControl 
	   :documentation "A descriptor for each kind of time control period.")
   (SetUp :initarg :SetUp
	   :initform nil
	   :accessor Setup
	   :documentation "The set-up status of the game.")
   (FEN :initarg :FEN
	   :initform nil
	   :accessor FEN
	   :documentation "The Forsyth-Edwards Notation for the starting position used in the game.")
   (Termination :initarg :Termination
	   :initform nil
	   :accessor Termination
	   :documentation "The reason for the conclusion of the game.")
   (Annotator :initarg :Annotator
	   :initform nil
	   :accessor Annotator
	   :documentation "this identifies the annotator(s) of the game.")
   (Mode :initarg :Mode
	   :initform nil
	   :accessor Mode
	   :documentation "The playing mode of the game such as OTB, PM, EM, ICS and TC")
   (PlyCount :initarg :PlyCount
	   :initform nil
	   :accessor PlyCount
	   :documentation "The number of ply (moves) in the game.")
;miscellaneous tags
   (misc :initarg :misc
	      :initform nil
	      :accessor misc
	      :documentation "list of cons  | nil  if not applicable   example (yourtag . tagvalue) ")
;movetext is a list of the moves including SAN, NAV, RAV et commentaries
   (Movetext :initarg :Movetext
	   :initform nil
	   :accessor Movetext
	   :documentation "Movetext is a list of the moves including SAN, NAV, RAV et commentaries.")   
 ))

(defmethod add-event-pgn ((obj portable-game-notation) ev)
  (if (equal (typep ev 'cons) T)
      (setf (Event obj) ev)
      (push ev (Event obj)))
  )

(defmethod add-site-pgn ((obj portable-game-notation) si)
  (if (equal (typep si 'cons) T)
      (setf (Site obj) si)
      (push si (Site obj)))
  )

(defmethod add-date-pgn ((obj portable-game-notation) dt)
  (if (equal (typep dt 'cons) T)
      (setf (Date obj) dt)
      (push dt (Date obj)))
  )

(defmethod add-round-pgn ((obj portable-game-notation) rd)
  (if (equal (typep rd 'cons) T)
      (setf (zRound obj) rd)
      (push rd (zRound obj)))
  )

(defmethod add-white-pgn ((obj portable-game-notation) wht)
  (if (equal (typep wht 'cons) T)
      (setf (White obj) wht)
      (push wht (White  obj)))
  )

(defmethod add-black-pgn ((obj portable-game-notation) blk)
  (if (equal (typep blk 'cons) T)
      (setf (Black obj) blk)
      (push blk (Black  obj)))
  )

(defmethod add-result-pgn ((obj portable-game-notation) re)
  (if (equal (typep re 'cons) T)
      (setf (Result obj) re)
      (push re (Result  obj)))
  )

(defmethod add-whitetitle-pgn ((obj portable-game-notation) wt)
  (if (equal (typep wt 'cons) T)
      (setf (WhiteTitle obj) wt)
      (push wt (WhiteTitle  obj)))
  )

(defmethod add-blacktitle-pgn ((obj portable-game-notation) bt)
  (if (equal (typep bt 'cons) T)
      (setf (BlackTitle obj) bt)
      (push bt (BlackTitle  obj)))
  )

(defmethod add-whiteelo-pgn ((obj portable-game-notation) we)
  (if (equal (typep we 'cons) T)
      (setf (WhiteElo obj) we)
      (push we (WhiteElo  obj)))
  )

(defmethod add-blackelo-pgn ((obj portable-game-notation) be)
  (if (equal (typep be 'cons) T)
      (setf (BlackElo obj) be)
      (push be (BlackElo  obj)))
  )

(defmethod add-whiteuscf-pgn ((obj portable-game-notation) wu)
  (if (equal (typep wu 'cons) T)
      (setf (WhiteUSCF obj) wu)
      (push wu (WhiteUSCF  obj)))
  )

(defmethod add-blackuscf-pgn ((obj portable-game-notation) bu)
  (if (equal (typep bu 'cons) T)
      (setf (BlackUSCF obj) bu)
      (push bu (BlackUSCF  obj)))
  )

(defmethod add-whitena-pgn ((obj portable-game-notation) wn)
  (if (equal (typep wn 'cons) T)
      (setf (WhiteNA obj) wn)
      (push wn (WhiteNA  obj)))
  )

(defmethod add-blackna-pgn ((obj portable-game-notation) bn)
  (if (equal (typep bn 'cons) T)
      (setf (BlackNA obj) bn)
      (push bn (BlackNA  obj)))
  )

(defmethod add-whitetype-pgn ((obj portable-game-notation) wy)
  (if (equal (typep wy 'cons) T)
      (setf (WhiteType obj) wy)
      (push wy (WhiteType  obj)))
  )

(defmethod add-blacktype-pgn ((obj portable-game-notation) by)
  (if (equal (typep by 'cons) T)
      (setf (BlackType obj) by)
      (push by (BlackType  obj)))
  )

(defmethod add-eventdate-pgn ((obj portable-game-notation) ed)
  (if (equal (typep ed 'cons) T)
      (setf (EventDate obj) ed)
      (push ed (EventDate  obj)))
  )

(defmethod add-eventsponsor-pgn ((obj portable-game-notation) es)
  (if (equal (typep es 'cons) T)
      (setf (EventSponsor obj) es)
      (push es (EventSponsor  obj)))
  )

(defmethod add-section-pgn ((obj portable-game-notation) se)
  (if (equal (typep se 'cons) T)
      (setf (Section obj) se)
      (push se (Section  obj)))
  )

(defmethod add-stage-pgn ((obj portable-game-notation) st)
  (if (equal (typep st 'cons) T)
      (setf (Stage obj) st)
      (push st (Stage  obj)))
  )

(defmethod add-board-pgn ((obj portable-game-notation) bd)
  (if (equal (typep bd 'cons) T)
      (setf (Board obj) bd)
      (push bd (Board  obj)))
  )

(defmethod add-opening-pgn ((obj portable-game-notation) op)
  (if (equal (typep op 'cons) T)
      (setf (Opening obj) op)
      (push op (Opening  obj)))
  )

(defmethod add-variation-pgn ((obj portable-game-notation) va)
  (if (equal (typep va 'cons) T)
      (setf (Variation obj) va)
      (push va (Variation  obj)))
  )

(defmethod add-subvariation-pgn ((obj portable-game-notation) sv)
  (if (equal (typep sv 'cons) T)
      (setf (SubVariation obj) sv)
      (push sv (SubVariation  obj)))
  )

(defmethod add-eco-pgn ((obj portable-game-notation) eco)
  (if (equal (typep eco 'cons) T)
      (setf (ECO obj) eco)
      (push eco (ECO  obj)))
  )

(defmethod add-nic-pgn ((obj portable-game-notation) nic)
  (if (equal (typep nic 'cons) T)
      (setf (NIC obj) nic)
      (push nic (NIC obj)))
  )

(defmethod add-time-pgn ((obj portable-game-notation) tm)
  (if (equal (typep tm 'cons) T)
      (setf (zTime obj) tm)
      (push tm (zTime obj)))
  )

(defmethod add-utctime-pgn ((obj portable-game-notation) ut)
  (if (equal (typep ut 'cons) T)
      (setf (UTCTime obj) ut)
      (push ut (UTCTime  obj)))
  )

(defmethod add-utcdate-pgn ((obj portable-game-notation) ud)
  (if (equal (typep ud 'cons) T)
      (setf (UTCDate obj) ud)
      (push ud (UTCDate  obj)))
  )

(defmethod add-timecontrol-pgn ((obj portable-game-notation) tc)
  (if (equal (typep tc 'cons) T)
      (setf (TimeControl obj) tc)
      (push tc (TimeControl  obj)))
  )

(defmethod add-setup-pgn ((obj portable-game-notation) sp)
  (if (equal (typep sp 'cons) T)
      (setf (SetUp obj) sp)
      (push sp (SetUp  obj)))
  )

(defmethod add-fen-pgn ((obj portable-game-notation) fen)
  (if (equal (typep fen 'cons) T)
      (setf (FEN obj) fen)
      (push fen (FEN  obj)))
  )

(defmethod add-termination-pgn ((obj portable-game-notation) te)
  (if (equal (typep te 'cons) T)
      (setf (Termination obj) te)
      (push te (Termination obj)))
  )

(defmethod add-annotator-pgn ((obj portable-game-notation) an)
  (if (equal (typep an 'cons) T)
      (setf (Annotator obj) an)
      (push an (Annotator  obj)))
  )

(defmethod add-mode-pgn ((obj portable-game-notation) md)
  (if (equal (typep md 'cons) T)
      (setf (Mode obj) md)
      (push md (Mode  obj)))
  )

(defmethod add-plycount-pgn ((obj portable-game-notation) pc)
  (if (equal (typep pc 'cons) T)
      (setf (PlyCount obj) pc)
      (push pc (PlyCount  obj)))
  )

(defun make-misc-pgn (yourtag  tagvalue)
   (return-from make-misc-pgn (cons yourtag tagvalue)) 
  )

(defmethod add-misc-pgn ((obj portable-game-notation) mc)
  (if (equal (typep mc 'cons) T)
      (push mc (misc obj)))
  )

(defmethod add-movetext-pgn ((obj portable-game-notation) mv)
  (if (equal (typep mv 'cons) T)
      (setf (Movetext obj) mv)
      (progn 
	(push mv (Movetext  obj))
	(setf (Movetext obj) (reverse (Movetext obj)))))
  )

(defmethod fill-slot-pgn ((obj portable-game-notation) slot-accessor item)
  (if (equal (typep item 'cons) T)      
      (setf (slot-value obj (read-from-string slot-accessor)) item)
      (push item (slot-value obj (read-from-string slot-accessor))))
  )

(defmethod erase-slot-pgn ((obj portable-game-notation) slot-accessor)
  (setf (slot-value obj (read-from-string slot-accessor)) nil)
 )

(defmethod remove-slotlist-pgn ((obj portable-game-notation) slot-accessor item)  ;remove an item from the list (string or cons)
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
      (return-from remove-slotlist-pgn t)	  
    ) 
  )

(defun make-chess-ref ()
  (let ((nd nil)(ls nil)(k 0))
    (dotimes (i (array-total-size *nd-ref*))
      (setf (aref *nd-ref* i) nil))
    (push (cons 0 0) (aref *nd-ref* 0))
    (dotimes (j (length *node*))
      (setf nd (elt *node* j))
      (setf ls (tl-split '(#\|) (getf nd :name)))
      ;(print ls)
      (if (equal (length ls) 3)
	  (progn
            (if (not (equal (parse-integer (elt ls 0) :junk-allowed t) nil))
		(progn
                  (setf k (read-from-string (elt ls 0)))
		  ;(print k)
		  (push (cons (getf nd :name) j) (aref *nd-ref* k))
		  ))
	    ))
      )
    )
  )

;nd-name already exists ==> do nothing else push cons with -1 as integer address
;because we don't know position in *node* and have no time to waste searching it
; b1 nil ==> push

(defun add-to-chess-ref (nd-name)
  (let ((ls nil)(k 0)(j -1)(b1 nil))
    (setf ls (tl-split '(#\|) nd-name))
    (if (not (equal (typep (aref *nd-ref* 0) 'cons) T))
	(progn
          (dotimes (i (array-total-size *nd-ref*))
            (setf (aref *nd-ref* i) nil))
	  (push (cons 0 0) (aref *nd-ref* 0))
	  ))
    (if (equal (length ls) 3)
	(progn
           (if (not (equal (parse-integer (elt ls 0) :junk-allowed t) nil))
	       (progn
                 (setf k (read-from-string (elt ls 0)))
		 (if (not (equal (typep (aref *nd-ref* k) 'cons) T))
		     (push (cons nd-name j) (aref *nd-ref* k))
		     (progn
                       (dolist (w (aref *nd-ref* k))
			 ;(print (car w))
                         (when (equal nd-name (car w))
			   (setf b1 T)
			   (return))
			 )
		       (if (equal b1 nil)(push (cons nd-name j) (aref *nd-ref* k)))
		       ))
		  ;(push (cons (getf nd :name) j) (aref *nd-ref* k))
		))
           ))
    (return-from add-to-chess-ref b1)
    )
  )

(defun make-simple-pgn (ev si dt rd wht blk re mvtxt)
  (let ((pgnobj nil))
    (setf pgnobj (make-instance 'portable-game-notation :Event (list ev) :Site (list si) :Date (list dt)
				:zRound (list rd) :White (list wht) :Black (list blk) :Result (list re) :Movetext mvtxt))
    (return-from make-simple-pgn pgnobj)
    )
  )

(defun make-full-pgn (ev si dt rd wht blk re wt bt we be wu bu wn bn wy by ed es se st bd op va sv eco nic tm ut ud tc sp fen te an md pc mc mvtxt)
  (let ((pgnobj nil))
    (setf pgnobj (make-simple-pgn ev si dt rd wht blk re mvtxt))
    (add-whitetitle-pgn pgnobj wt)
    (add-blacktitle-pgn pgnobj bt)
    (add-whiteelo-pgn pgnobj we)
    (add-blackelo-pgn pgnobj be)
    (add-whiteuscf-pgn pgnobj wu)
    (add-blackuscf-pgn pgnobj bu)
    (add-whitena-pgn pgnobj wn)
    (add-blackna-pgn pgnobj bn)
    (add-whitetype-pgn pgnobj wy)
    (add-blacktype-pgn pgnobj by)
    (add-eventdate-pgn pgnobj ed)
    (add-eventsponsor-pgn pgnobj es)
    (add-section-pgn pgnobj se)
    (add-stage-pgn pgnobj st)
    (add-board-pgn pgnobj bd)
    (add-opening-pgn pgnobj op)
    (add-variation-pgn pgnobj va)
    (add-subvariation-pgn pgnobj sv)
    (add-eco-pgn pgnobj eco)
    (add-nic-pgn pgnobj nic)
    (add-time-pgn pgnobj tm)
    (add-utctime-pgn pgnobj ut)
    (add-utcdate-pgn pgnobj ud)
    (add-timecontrol-pgn pgnobj tc)
    (add-setup-pgn pgnobj sp)
    (add-fen-pgn pgnobj fen)
    (add-termination-pgn pgnobj te)
    (add-annotator-pgn pgnobj an)
    (add-mode-pgn pgnobj md)
    (add-plycount-pgn pgnobj pc)
    (add-misc-pgn pgnobj mc)
    (return-from make-full-pgn pgnobj)
    )
  )

    ;(setf p (make-pathname :directory '(:absolute "home" "arkho" ".emacs.d" "chess-games" "wch") :name fs))
    ;(setf p (make-pathname :directory '(:absolute "home" "arkho") :name fs))
    ;(setf p (make-pathname :directory '(:absolute "home" "arkho" ".emacs.d" "chess-games" "wch") :name "filepgn.pgn"))

(defun pgn-from-file (pt pg)
  (let ((pgnobj nil)(p2 nil)(p3 nil)(p4 nil)(b1 nil)(ptag nil)(pgame nil)(lw nil))
    (setf ptag (replace-all pt '(#\return) " "))
    (setf ptag (replace-all ptag '(#\linefeed) " "))
    (setf ptag (replace-all ptag '(#\newline) " "))
    (setf pgame (replace-all pg '(#\return) " "))
    (setf pgame (replace-all pgame '(#\linefeed) " "))
    (setf pgame (replace-all pgame '(#\newline) " "))
    ;(print ptag)(print pgame)
    (setf lw  (ppcre:all-matches-as-strings "\\[[^\\[\\]]+\\]" ptag))
    ;(print lw)
    (setf pgnobj (make-simple-pgn "?" "?" "?" "?" "?" "?" "?" pgame))
    (dolist (w lw)
      (setf p2 (ppcre:all-matches-as-strings "§[^§]+§" w))(setf p3 nil)(setf b1 nil)(setf p4 nil)
      (if (not (equal p2 nil))
	  (progn
	    (setf p3 (elt p2 0))
	    (setf p3 (replace-all p3 '(#\§) ""))
	    ))
      ;(print w)(print p3)
      (if (not (equal (search "Event " w) nil))
	  (progn
            (add-event-pgn pgnobj (list p3))(setf b1 t)
	    ))
      (if (not (equal (search "Site " w) nil))
	  (progn
            (add-site-pgn pgnobj (list p3))(setf b1 t)
	    ))
      (if (not (equal (search "Date " w) nil))
	  (progn
            (add-date-pgn pgnobj (list p3))(setf b1 t)
	    ))
      (if (not (equal (search "Round " w) nil))
	  (progn
            (add-round-pgn pgnobj (list p3))(setf b1 t)
	    ))
      (if (not (equal (search "White " w) nil))
	  (progn
            (add-white-pgn pgnobj (list p3))(setf b1 t)
	    ))
      (if (not (equal (search "Black " w) nil))
	  (progn
            (add-black-pgn pgnobj (list p3))(setf b1 t)
	    ))
      (if (not (equal (search "Result " w) nil))
	  (progn
            (add-result-pgn pgnobj (list p3))(setf b1 t)
	    ))
      (if (not (equal (search "WhiteTitle " w) nil))
	  (progn
            (add-whitetitle-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "BlackTitle " w) nil))
	  (progn
            (add-blacktitle-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "WhiteElo " w) nil))
	  (progn
            (add-whiteelo-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "BlackElo " w) nil))
	  (progn
            (add-blackelo-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "WhiteUSCF " w) nil))
	  (progn
            (add-whiteuscf-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "BlackUSCF " w) nil))
	  (progn
            (add-blackuscf-pgn pgnobj p3)(setf b1 t)
	    ))      
      (if (not (equal (search "WhiteNA " w) nil))
	  (progn
            (add-whitena-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "BlackNA " w) nil))
	  (progn
            (add-blackna-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "WhiteType " w) nil))
	  (progn
            (add-whitetype-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "BlackType " w) nil))
	  (progn
            (add-blacktype-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "EventDate " w) nil))
	  (progn
            (add-eventdate-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "EventSponsor " w) nil))
	  (progn
            (add-eventsponsor-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "Section " w) nil))
	  (progn
            (add-section-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "Stage " w) nil))
	  (progn
            (add-stage-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "Board " w) nil))
	  (progn
            (add-board-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "Opening " w) nil))
	  (progn
            (add-opening-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "Variation " w) nil))
	  (progn
            (add-variation-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "SubVariation " w) nil))
	  (progn
            (add-subvariation-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "ECO " w) nil))
	  (progn
            (add-eco-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "NIC " w) nil))
	  (progn
            (add-nic-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "Time " w) nil))
	  (progn
            (add-time-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "UTCTime " w) nil))
	  (progn
            (add-utctime-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "UTCDate " w) nil))
	  (progn
            (add-utcdate-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "TimeControl " w) nil))
	  (progn
            (add-timecontrol-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "SetUp " w) nil))
	  (progn
            (add-setup-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "FEN " w) nil))
	  (progn
            (add-fen-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "Termination " w) nil))
	  (progn
            (add-termination-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "Annotator " w) nil))
	  (progn
            (add-annotator-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "Mode " w) nil))
	  (progn
            (add-mode-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (not (equal (search "PlyCount " w) nil))
	  (progn
            (add-plycount-pgn pgnobj p3)(setf b1 t)
	    ))
      (if (equal b1 nil)
	  (progn
            (setf p4  (ppcre:all-matches-as-strings "[\\w_-]+" w))
	    (if (not (equal p4 nil))
		(add-misc-pgn pgnobj (make-misc-pgn (elt p4 0) p3)))
	    ))
      )
    (describe pgnobj)
    (return-from pgn-from-file pgnobj)
    )
  )

(defun transres (res)
  (let ((s1 nil))
    (if (not (equal (search "1-0" res) nil))(setf s1 "W"))
    (if (not (equal (search "0-1" res) nil))(setf s1 "B"))
    (if (not (equal (search "1/2-1/2" res) nil))(setf s1 "N"))
    (return-from transres s1)
    )
  )

(defun make-fpgn-name(pgnobj)
  (let ((fs nil))
    (setf fs (concatenate 'string (elt (slot-value pgnobj (read-from-string "Event")) 0)
			  "|" (elt (slot-value pgnobj (read-from-string "Date")) 0)
			  "|" (elt (slot-value pgnobj (read-from-string "White")) 0)
			  "|" (elt (slot-value pgnobj (read-from-string "Black")) 0)
			  "|" (elt (slot-value pgnobj (read-from-string "zRound")) 0)
			  "|" (transres (elt (slot-value pgnobj (read-from-string "Result")) 0))
			  ))
    ;(print fs)
    (setf fs (replace-all fs "?" "x"))
    (setf fs (replace-all fs *path-delim* " "))
    (return-from make-fpgn-name fs)
    ) 
  )

(defun pgn-to-file (dir pgnobj)
  (let ((fn nil))
    ;(setf s1 "testpgn")
    (setf fn (make-fpgn-name pgnobj))
    (if (not (equal (subseq dir (- (length dir) 1)) *path-delim*))(setf dir (concatenate 'string  dir *path-delim*)))
     (with-open-file (stream (concatenate 'string dir  (string-trim "\"" fn) ".pgn") :direction :output :if-exists :supersede)
        (write-line (concatenate 'string "[Event " "\"" (elt (slot-value pgnobj (read-from-string "Event")) 0) "\"]") stream)
        (write-line (concatenate 'string "[Site " "\"" (elt (slot-value pgnobj (read-from-string "Site")) 0) "\"]") stream)
        (write-line (concatenate 'string "[Date " "\"" (elt (slot-value pgnobj (read-from-string "Date")) 0) "\"]") stream)
        (write-line (concatenate 'string "[Round " "\"" (elt (slot-value pgnobj (read-from-string "zRound")) 0) "\"]") stream)
        (write-line (concatenate 'string "[White " "\"" (elt (slot-value pgnobj (read-from-string "White")) 0) "\"]") stream)
        (write-line (concatenate 'string "[Black " "\"" (elt (slot-value pgnobj (read-from-string "Black")) 0) "\"]") stream)
        (write-line (concatenate 'string "[Result " "\"" (elt (slot-value pgnobj (read-from-string "Result")) 0) "\"]") stream)
        (if (not (equal (slot-value pgnobj (read-from-string "WhiteTitle")) nil))
	    (write-line (concatenate 'string "[WhiteTitle " "\"" (elt (slot-value pgnobj (read-from-string "WhiteTitle")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "BlackTitle")) nil))
	    (write-line (concatenate 'string "[BlackTitle " "\"" (elt (slot-value pgnobj (read-from-string "BlackTitle")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "WhiteElo")) nil))
	    (write-line (concatenate 'string "[WhiteElo " "\"" (elt (slot-value pgnobj (read-from-string "WhiteElo")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "BlackElo")) nil))
	    (write-line (concatenate 'string "[BlackElo " "\"" (elt (slot-value pgnobj (read-from-string "BlackElo")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "WhiteUSCF")) nil))
	    (write-line (concatenate 'string "[WhiteUSCF " "\"" (elt (slot-value pgnobj (read-from-string "WhiteUSCF")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "BlackUSCF")) nil))
	    (write-line (concatenate 'string "[BlackUSCF " "\"" (elt (slot-value pgnobj (read-from-string "BlackUSCF")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "WhiteNA")) nil))
	    (write-line (concatenate 'string "[WhiteNA " "\"" (elt (slot-value pgnobj (read-from-string "WhiteNA")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "BlackNA")) nil))
	    (write-line (concatenate 'string "[BlackNA " "\"" (elt (slot-value pgnobj (read-from-string "BlackNA")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "WhiteType")) nil))
	    (write-line (concatenate 'string "[WhiteType " "\"" (elt (slot-value pgnobj (read-from-string "WhiteType")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "BlackType")) nil))
	    (write-line (concatenate 'string "[BlackType " "\"" (elt (slot-value pgnobj (read-from-string "BlackType")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "EventDate")) nil))
	    (write-line (concatenate 'string "[EventDate " "\"" (elt (slot-value pgnobj (read-from-string "EventDate")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "EventSponsor")) nil))
	    (write-line (concatenate 'string "[EventSponsor " "\"" (elt (slot-value pgnobj (read-from-string "EventSponsor")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "Section")) nil))
	    (write-line (concatenate 'string "[Section " "\"" (elt (slot-value pgnobj (read-from-string "Section")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "Stage")) nil))
	    (write-line (concatenate 'string "[Stage " "\"" (elt (slot-value pgnobj (read-from-string "Stage")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "Board")) nil))
	    (write-line (concatenate 'string "[Board " "\"" (elt (slot-value pgnobj (read-from-string "Board")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "Opening")) nil))
	    (write-line (concatenate 'string "[Opening " "\"" (elt (slot-value pgnobj (read-from-string "Opening")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "Variation")) nil))
	    (write-line (concatenate 'string "[Variation " "\"" (elt (slot-value pgnobj (read-from-string "Variation")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "SubVariation")) nil))
	    (write-line (concatenate 'string "[SubVariation " "\"" (elt (slot-value pgnobj (read-from-string "SubVariation")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "ECO")) nil))
	    (write-line (concatenate 'string "[ECO " "\"" (elt (slot-value pgnobj (read-from-string "ECO")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "NIC")) nil))
	    (write-line (concatenate 'string "[NIC " "\"" (elt (slot-value pgnobj (read-from-string "NIC")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "zTime")) nil))
	    (write-line (concatenate 'string "[Time " "\"" (elt (slot-value pgnobj (read-from-string "zTime")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "UTCTime")) nil))
	    (write-line (concatenate 'string "[UTCTime " "\"" (elt (slot-value pgnobj (read-from-string "UTCTime")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "UTCDate")) nil))
	    (write-line (concatenate 'string "[UTCDate " "\"" (elt (slot-value pgnobj (read-from-string "UTCDate")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "TimeControl")) nil))
	    (write-line (concatenate 'string "[TimeControl " "\"" (elt (slot-value pgnobj (read-from-string "TimeControl")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "SetUp")) nil))
	    (write-line (concatenate 'string "[SetUp " "\"" (elt (slot-value pgnobj (read-from-string "SetUp")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "FEN")) nil))
	    (write-line (concatenate 'string "[FEN " "\"" (elt (slot-value pgnobj (read-from-string "FEN")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "Termination")) nil))
	    (write-line (concatenate 'string "[Termination " "\"" (elt (slot-value pgnobj (read-from-string "Termination")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "Annotator")) nil))
	    (write-line (concatenate 'string "[Annotator " "\"" (elt (slot-value pgnobj (read-from-string "Annotator")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "Mode")) nil))
	    (write-line (concatenate 'string "[Mode " "\"" (elt (slot-value pgnobj (read-from-string "Mode")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "PlyCount")) nil))
	    (write-line (concatenate 'string "[PlyCount " "\"" (elt (slot-value pgnobj (read-from-string "PlyCount")) 0) "\"]") stream))
	(if (not (equal (slot-value pgnobj (read-from-string "misc")) nil))
	    (progn
              (dolist (z (reverse (slot-value pgnobj (read-from-string "misc"))))
                (write-line (concatenate 'string "[" (car z)  " \"" (cdr z)  "\"]") stream)
		)
	      ))
	(if (not (equal (slot-value pgnobj (read-from-string "Movetext")) nil))
	    (write-line (slot-value pgnobj (read-from-string "Movetext")) stream))
       (terpri stream)
       (close stream)))
  (return-from pgn-to-file t))

(defun mv-to-list (pres pgame)
  (let ((ls nil)(s1 nil)(mv nil)(i 0)(j 0)(k1 0)(k2 0)(rs nil))
    (setf mv (replace-all pgame '(#\return) " "))
    (setf mv (string-left-trim '(#\Space)  mv))
    (setf mv (replace-all mv '(#\linefeed) " "))
    (setf mv (replace-all mv '(#\newline) " "))
    (setf mv (replace-all mv "." ". "))     ;pas de mouvement avec xx.W B
    (setf rs pres)(setf pres rs)
    ;todo remove comments, annotations
    ;(print (length mv))
    (loop while(not (equal (search ". " mv :start2 i) nil))
       do
	 (setf j (search ". " mv :start2 i))
         (if (< j 2)(setf k1 0)
	    (progn
              (if (not (equal (is-pnum (aref mv (- j 1))) nil))
                  (progn
               	    (setf k2 (- j 1))
                    (if (not (equal (is-pnum (aref mv (- j 2))) nil))
                        (progn
			  (setf k2 (- j 2))
			  (if (not (equal (is-pnum (aref mv (- j 3))) nil))(setf k2 (- j 3)))
			  ))
		    ))
	      ))
	 (if (> k2 k1)
	     (progn
               (setf s1 (subseq mv k1 k2))
	       (push s1 ls)
	       (setf k1 k2)
	       (setf k2 0)
	       ;(print s1)
	       ))
	 
	 ;(print j)
	 (setf i (+ j 1))			    
	 )
      (setf s1 (subseq mv k1))
      (push s1 ls)
      (setf ls (reverse ls))
      ;(print ls)
      (return-from mv-to-list ls)
      ;(print s1)
    )
  )

(defun create-nd-chess (nd)
  (let ((b nil))
    (if (not (equal (search "0-1" nd) nil))(return-from create-nd-chess (create-node "0-1")))
    (if (not (equal (search "1-0" nd) nil))(return-from create-nd-chess (create-node "1-0")))
    (if (not (equal (search "1/2-1/2" nd) nil))(return-from create-nd-chess (create-node "1/2-1/2")))
     (if (equal (add-to-chess-ref nd) nil)
         (setf b (create-node2-fast nd "passthru" 0)))
     (return-from create-nd-chess b)
   )
  )

(defun add-to-neural-network (gname mvls pres)
  (let ((mvx nil)(nr nil)(n1 nil)(n2 nil)(n3 nil)(n-old nil))
    (create-seed-sense gname 1 1 -1)
    (dolist (w mvls)
      (setf mvx (ppcre:all-matches-as-strings "[^\\s]+" w))
      (print mvx)
      (if (> (length mvx) 2)
	  (progn
 	    (setf nr (subseq (elt mvx 0) 0 (- (length (elt mvx 0)) 1)))
	    (setf n1 (concatenate 'string nr "|W|" (elt mvx 1)))

	    (if (not (equal (search pres (elt mvx 1)) nil))
		(progn
		  (setf n1 pres)
		  (setf n2 nil))
                (setf n1 (concatenate 'string nr "|W|" (elt mvx 1))))
	    
	    (if (not (equal (search pres (elt mvx 2)) nil))
		(setf n2 pres)
		(setf n2 (concatenate 'string nr "|B|" (elt mvx 2))))
		
	    (setf n1 (string-trim '(#\Space)  n1))
	    (setf n2 (string-trim '(#\Space)  n2))
	    ;(print n1)
	    ;(print n2)
	    ;(if (not (equal (search pres n2) nil))(setf n2 (elt mvx 2)))
            (if (equal (create-nd-chess n1) t)(add-seed-to-node gname n1)
		(progn
                   (if (equal (check-seed-from-node n1 gname) nil)(add-seed-to-node gname n1))
		   ))
	    (if (and (not (equal n-old nil)) (equal (check-next-from-node n-old n1) nil))
		(add-next-to-node n1 n-old))
            (if (equal (create-nd-chess n2) t)(add-seed-to-node gname n2)
		(progn
                   (if (equal (check-seed-from-node n2 gname) nil)(add-seed-to-node gname n2))
		   ))
	    (if (equal (check-next-from-node n1 n2) nil)(add-next-to-node n2 n1))
	    (if (equal (length mvx) 4) 
		(progn
		  (if (not (equal (search pres (elt mvx 3)) nil))
		      (progn
			(setf n3 (elt mvx 3))
			(setf n3 (string-trim '(#\Space)  n3))
                        (create-node n3)
			(if (equal (check-seed-from-node n3 gname) nil)(add-seed-to-node gname n3))
		        (if (equal (check-next-from-node n2 n3) nil)(add-next-to-node n3 n2))
			))))
	    
	    (setf n-old n2)
	    (setf n1 nil)(setf n2 nil)(setf n3 nil)
	      ))
      )
    )
  )

(defun load-chess-nn (nn-path)
  (load nn-path :if-does-not-exist nil)
  (make-chess-ref)
  (c-init-board *chess-board*)
  )

; (extract-xpgn "/home/arkho/.emacs.d/chess-games/players/fischer/St Stefan Belgrade m|1992.xx.xx|Spassky, Boris V|Fischer, Robert James|30|B.pgn" "/home/arkho/.emacs.d/chess-games/players/fischer/" nil t nil)
; (extract-xpgn "/home/arkho/.emacs.d/chess-games/import/WorldChamp2018.pgn" "/home/arkho/.emacs.d/chess-games/wch/" nil nil t)
; (extract-xpgn "/home/arkho/.emacs.d/chess-games/import/morphy/Morphy.pgn" "/home/arkho/.emacs.d/chess-games/players/morphy/" t t nil)
; "\\[[\\w\\s_-]+\\]"    (ppcre:all-matches-as-strings "\\[[^\\[\\]]+\\]" ts2)
;(defvar ts1 "[Event \"WCh 2018\"][Site \"London ENG\"][ECO \"B31\"][EventDate \"2018.11.09\"]1. e4 c5 2. Nf3 Nc6 3. Bb5 g6 4. Bxc6+")
;att %cvar% dans outdir est remplacé par eco/ecocomplet

(defun extract-xpgn (fs outdir &optional (import nil)(neural-network nil) (ask4next nil))    ;import :create pgn file for one game ask4next : ask to stop or continue 
  (let ((i 0)(j1 0)(j2 0)(j3 0)(g 0)(pgnobj nil)(ptag nil)(pgame nil)(pres nil)(gname nil)(tx nil)
	(mvls nil)(eco nil)(cvar nil)(ecd nil)(cv 0)(tgdir nil)(ls nil)(b1 nil))
       (with-open-file (stream fs
                      :if-does-not-exist nil
                      :external-format '(:utf-8 :replacement "?"))
	 (when stream
            (loop for line = (read-line stream nil)
              while line do
	      (if (and (>= i 0) (> (length line) 0))    ;1st line ok
		  (progn
		    (setf tgdir outdir)
		    (if (or (not (equal (search "[" line) nil)) (not (equal (search "]" line) nil)))
			(progn
			  (if (not (equal (subseq line 0 1) "%"))(setf ptag (concatenate 'string ptag line))) 
			  (if (not (equal (search "Result" line) nil))
			      (progn
                                (setf j1 (search "Result" line))
				(if (not (equal (search "\"" line :start2 j1) nil))(setf j2 (search "\"" line :start2 j1)))
				(if (and (> j2 j1) (not (equal (search "\"" line :start2 (+ j2 1)) nil)))(setf j3 (search "\"" line :start2 (+ j2 1))))
				(if (> j3 j2)(setf pres (subseq line (+ j2 1) j3)))
				))
			  )
			(progn
			  (if (not (equal (subseq line 0 1) "%"))(setf pgame (concatenate 'string pgame line " "))) 
			  (if (and (> (length pres) 0) (not (equal (search pres line) nil)))
			      (progn
					;(print ptag)(print pres)(print pgame)
				(setf ptag (replace-all ptag "\"" "§"))
				(setf pgnobj (pgn-from-file ptag pgame))
				(setf mvls (mv-to-list pres pgame))
				(setf gname (make-fpgn-name pgnobj))
				(setf eco nil)
				(if (not (equal (slot-value pgnobj (read-from-string "ECO")) nil))
				    (setf eco (elt (slot-value pgnobj (read-from-string "ECO")) 0)))
				;(print eco)
				(if (not (equal (search "%cvar%" outdir) nil))
				    (progn
				      (setf cv 1)
				      (if (equal eco nil)(setf cvar (get-unk-cvar pgame))
					                 (progn
                                                           (setf cvar (get-stand-cvar2 pgame eco))
							   (setf cv 2)))
				      ;(wrong eco)
                                      (if (and (equal cvar nil)(equal cv 2))(setf cvar (get-unk-cvar pgame)))
				      (if (not (equal cvar nil))
					  (setf ecd (elt (cdr (elt cvar 0)) 0))(setf ecd "99"))
				      (if (> (length ecd) 3)(setf ecd (concatenate 'string (subseq ecd 0 3) *path-delim* ecd)))
				      (setf tgdir (replace-all outdir "%cvar%" ecd))
				      ))
				(setf b1 nil)
                                (dolist (z ls)
                                   (if (equal eco z)(progn
                                                    (setf b1 T)
			                            (return)
			                            ))
	                         )
                                (if (equal b1 nil)(push eco ls))
				
				(if (equal import t)
				    (progn 
				      (pgn-to-file tgdir pgnobj)
				      ))
				(if (equal neural-network t)
				    (progn
                                      (add-to-neural-network gname mvls pres)
				      ))
				(if (equal ask4next t)
				    (progn
				      (format t "~&Would you like to continue (next game) ? ")
				      (setf tx (with-output-to-string (s)(format s "~a" (read-line))))
				    ))
				(setf j1 0)(setf j2 0)(setf j3 0)(setf ptag nil)(setf pgame nil)(setf pgnobj nil)
				(setf g (+ g 1))
				(setf tx (string-trim "\"" tx))
                                (setf tx (replace-all tx '(#\return) ""))
                                (setf tx (replace-all tx '(#\linefeed) ""))
				;(print tx)(print (length tx))
				(if (or (equal tx "N")(equal tx "n")(equal tx "No"))
				    (return-from extract-xpgn g))
				(setf tx nil)
				))
			  )
			)
	     ))
	  (setf i (+ i 1)))
       (close stream)))
      (return-from extract-xpgn ls)
    )
  )

;(import-pgn "/home/arkho/.emacs.d/chess-games/cg" "/home/arkho/.emacs.d/chess-games/wch")
; (import-pgn "/home/arkho/.emacs.d/chess-games/import/lasker" "/home/arkho/.emacs.d/chess-games/players/lasker")
; (import-pgn "/home/arkho/.emacs.d/chess-games/import/anand" "/home/arkho/.emacs.d/chess-games/players/anand")

(defun import-pgn (zpath-in zpath-out)
  (let ((fn nil)(s1 nil))
    (print (current-datetime-string))
    (if (not (equal (subseq zpath-in (- (length zpath-in) 1)) *path-delim*))(setf zpath-in (concatenate 'string  zpath-in *path-delim*)))
    (if (not (equal (subseq zpath-out (- (length zpath-out) 1)) *path-delim*))(setf zpath-out (concatenate 'string  zpath-out *path-delim*)))
    (setf s1 (concatenate 'string zpath-in "*.pgn"))
    (setf fn (directory s1))
    (dolist (z fn)
      (print (namestring z))
      (extract-xpgn (namestring z) zpath-out t t nil)
      ;(extract-xpgn (namestring z) zpath-out nil t nil)
      )
    (print (current-datetime-string))
    (return-from import-pgn (concatenate 'string (format nil "~a" (length fn)) " files processed"))
    )
  )

; (import-pgn2 "/home/arkho/.emacs.d/chess-games/imported" "/home/arkho/.emacs.d/chess-games/openings/%cvar%")
(defun import-pgn2 (zpath-in zpath-out)
  (let ((fn nil)(s1 nil)(ls nil)(w nil))
    (print (current-datetime-string))
    (if (not (equal (subseq zpath-in (- (length zpath-in) 1)) *path-delim*))(setf zpath-in (concatenate 'string  zpath-in *path-delim*)))
    (if (not (equal (subseq zpath-out (- (length zpath-out) 1)) *path-delim*))(setf zpath-out (concatenate 'string  zpath-out *path-delim*)))
    (setf s1 (concatenate 'string zpath-in "*.pgn"))
    (setf fn (directory s1))
    (dolist (z fn)
      (print (namestring z))
      (setf w (extract-xpgn (namestring z) zpath-out t nil nil))
      (setf ls (append ls w))
      )
    (print (current-datetime-string))
    (return-from import-pgn2 ls)
    )
  )

;(import-pgn3 "/home/arkho/.emacs.d/chess-games/openings" "/home/arkho/.emacs.d/chess-games/clusters" "A42")
(defun import-pgn3 (zpath-in zpath-out eco)
  (let ((d1 nil)(d2 nil)(s1 nil)(ls nil)(lx nil)(s2 nil)(s3 nil)(s4 nil)(s5 nil)(s6 nil)(s7 nil)
	(fn nil)(drk1 nil)(drk2 nil)(b1 nil)(b2 nil)(m 0))
    (print (current-datetime-string))
    (if (not (equal (subseq zpath-in (- (length zpath-in) 1)) *path-delim*))(setf zpath-in (concatenate 'string  zpath-in *path-delim*)))
    (if (not (equal (subseq zpath-out (- (length zpath-out) 1)) *path-delim*))(setf zpath-out (concatenate 'string  zpath-out *path-delim*)))
    (setf ls nil)(setf eco eco)
    (setf s1 (concatenate 'string zpath-in "*"))
    (setf d1 (directory s1))
    (dolist (z d1)
     (print z)
      (setf s2 (namestring z))
      (if (equal (subseq s2 (- (length s2) 1)) *path-delim*)
	  (progn
            (setf s2 (concatenate 'string s2 "*"))
            (setf d2 (directory s2))
	    (dolist (w d2)
              ;(print w)
	      (setf s5 (namestring w))
              (setf lx (pathname-directory s5))
              (setf b1 nil)
              (if (> (length lx) 1)
	          (progn
                    (setf s3 (elt lx (- (length lx) 1)))
	            (setf m (length eco))
	            (if (equal m 0)(setf b1 T)
		        (progn
		          (if (<= m (length s3))
		             (progn
			       (setf s4 (subseq s3 0 m))
                               (if (equal s4 eco)(setf b1 T))
			  ))		  
		     ))
	        ))
               ;b1 T implique do import
	      (if (equal b1 T)(progn
                                (print w)
				(setf s6 (concatenate 'string (subseq s3 0 3) *path-delim* s3 *path-delim*))
				(setf drk1 (concatenate 'string zpath-in s6))
				(setf drk2 (concatenate 'string zpath-out s6))
				(print drk1) (print drk2) 
			        (setf s7 (concatenate 'string drk1 "*.pgn"))
                                (setf fn (directory s7))
				(zvar)
                                  (dolist (y fn)
                                    (print (namestring y))
                                    (extract-xpgn (namestring y) drk2 nil t nil)
                                    
                                    )
				(save-graph-v4 (concatenate 'string drk2 s3 ".lisp"))  
				(setf b2 nil)
                                (dolist (x ls)
                                   (if (equal s6 x)(progn
                                                    (setf b2 T)
			                            (return)
			                            ))
	                           )
				(if (equal b2 nil)(push s6 ls))
				))
	     )
	    ))
      )
    (print (current-datetime-string))
    (return-from import-pgn3 ls)
    )
  )

(defun import-batch ()
  ;(import-pgn3 "/home/arkho/.emacs.d/chess-games/openings" "/home/arkho/.emacs.d/chess-games/clusters" "A00")
  ;(import-pgn3 "/home/arkho/.emacs.d/chess-games/openings" "/home/arkho/.emacs.d/chess-games/clusters" "A01")
  ;(import-pgn3 "/home/arkho/.emacs.d/chess-games/openings" "/home/arkho/.emacs.d/chess-games/clusters" "A02")
  ;(import-pgn3 "/home/arkho/.emacs.d/chess-games/openings" "/home/arkho/.emacs.d/chess-games/clusters" "A03")
  ;(import-pgn3 "/home/arkho/.emacs.d/chess-games/openings" "/home/arkho/.emacs.d/chess-games/clusters" "A04")
  ;(import-pgn3 "/home/arkho/.emacs.d/chess-games/openings" "/home/arkho/.emacs.d/chess-games/clusters" "A05")
  ;(import-pgn3 "/home/arkho/.emacs.d/chess-games/openings" "/home/arkho/.emacs.d/chess-games/clusters" "A06")
  ;(import-pgn3 "/home/arkho/.emacs.d/chess-games/openings" "/home/arkho/.emacs.d/chess-games/clusters" "A07")
  ;(import-pgn3 "/home/arkho/.emacs.d/chess-games/openings" "/home/arkho/.emacs.d/chess-games/clusters" "A08")
  ;(import-pgn3 "/home/arkho/.emacs.d/chess-games/openings" "/home/arkho/.emacs.d/chess-games/clusters" "A09")
  
  )

(defun find-move (num bw mv)    ; exampe 1 "B" "e4"
  (let ((s1 nil)(ls nil)(i-node -1))
    (setf s1 (concatenate 'string (format nil "~a" num) "|" bw "|" mv))
    (if (> num (- (array-total-size *nd-ref*) 1))(return-from find-move i-node))
    ;(print s1)
    (setf ls (aref *nd-ref* num))
    (dolist (w ls)
      (when (equal s1 (car w))
	(setf i-node (cdr w))
	(return)))
    (return-from find-move i-node)
      )
  )

(defun find-game1 (name)
 (let ((ls nil))
  (dolist (sd *seed*)
    ;(when (equal (format nil "~(~a~)" (getf sd :name)) (format nil "~(~a~)" name))
    (when (not (equal (search name (getf sd :name)) nil))
        (push (getf sd :name) ls))
    )
  (return-from find-game1 ls)
  ))

(defun is-move-in-seg (sg segment)
  (let ((rs nil))
    (dolist (w segment)
      (if (equal w sg)(progn
                        (setf rs T)
			(return)
			))
      )
    (return-from is-move-in-seg rs)
    )
  )

(defun add-move-to-seg (direction segment num bw mv) ;direction "-" down or "+" up segment is a list (nil allowed) num bw mv example 3 "W" "Nf3" 
  (let ((s1 nil)(ls nil))
    (setf s1 (concatenate 'string (format nil "~a" num) "|" bw "|" mv))
    (if (not (equal segment nil))(setf ls segment))
    (if (equal direction "+" )(setf ls (append ls (list s1))))
    (if (equal direction "-")(push s1 ls))
    (return-from add-move-to-seg ls)
    )
  )

;example (get-segment-forward "FIDE-Wch|1993.xx.xx|Karpov, Anatoly|Timman, Jan H|18|N" 1 "W" "d4" 5 "W" "f3")

(defun get-segment-forward (sdname num1 bw1 mv1 num2 bw2 mv2) ;sdname is seed name 1 start 2 end
  (let ((s1 nil)(s2 nil)(ls nil)(i-node -1)(j-node -1)(b1 nil)(cpt 0)(max 0)(nd nil)(lx nil)(k -1))
    (setf s1 s2)
    (setf s2 s1)
    (setf s1 (concatenate 'string (format nil "~a" num1) "|" bw1 "|" mv1))
    (setf s2 (concatenate 'string (format nil "~a" num2) "|" bw2 "|" mv2))
    (setf i-node (find-move num1 bw1 mv1))
    (setf j-node (find-move num2 bw2 mv2))
    (setf max (*  (+ (abs (- num2 num1)) 1) 2))
    
    (if (> i-node -1) (setf ls (add-move-to-seg "+" ls num1 bw1 mv1))
	(return-from get-segment-forward nil))
    (if (equal (check-setseed i-node sdname) nil)
	(return-from get-segment-forward nil))
    (if (equal i-node j-node)
	(return-from get-segment-forward ls))
     (loop while(equal b1  nil)
	do
	  (setf cpt (+ cpt 1))
	  (if (> cpt max)(setf b1 T))	  
	  (if (equal b1 T)(return))
	  (setf nd (elt *node* i-node))
	  (dotimes (i (length (getf nd :set-next)))
            (setf lx (tl-split '(#\|) (elt (getf nd :set-next) i)))
	    (if (equal (length lx) 3)
		(progn
                  (setf k (find-move (read-from-string (elt lx 0)) (elt lx 1) (elt lx 2)))
		  (if (and (> k -1) (equal (check-setseed k sdname) T))
		      (progn
			(if (<= (read-from-string (elt lx 0)) num2)
			    (setf ls (add-move-to-seg "+" ls (read-from-string (elt lx 0)) (elt lx 1) (elt lx 2))))
			(setf i-node k)
			(return)
			))
		  ))
	    )
	   (if (equal i-node j-node)(setf b1 T))
	  )
     (return-from get-segment-forward ls)
    )
  )

(defun cmp-segments (sg1 sg2)
  (let ((b1 T))
    (if (not (equal (length sg1) (length sg2)))(return-from cmp-segments nil))
    (dotimes (i (length sg1))
      (if (not (equal (elt sg1 i) (elt sg2 i)))
	  (progn
            (setf b1 nil)
	    (return)
	    ))
      )
    (return-from cmp-segments b1)
    )
  )

(defun get-segment-group (segment)
  (let ((nd nil)(ls nil)(lx nil)(i-node -1)(num1 nil)(bw1 nil)(mv1 nil)(num2 nil)(bw2 nil)(mv2 nil)(b1 nil))
    (if (equal segment nil)(return-from get-segment-group nil))
    
    (setf lx (tl-split '(#\|) (elt segment 0)))
    (if (equal (length lx) 3)
	(progn
          (setf num1 (read-from-string (elt lx 0)))
	  (setf bw1 (elt lx 1))
	  (setf mv1 (elt lx 2))
	  ))
    
    (setf lx (tl-split '(#\|) (elt segment (- (length segment) 1))))
    (if (equal (length lx) 3)
	(progn
          (setf num2 (read-from-string (elt lx 0)))
	  (setf bw2 (elt lx 1))
	  (setf mv2 (elt lx 2))
	  ))    
    ;(print segment)
    (if (equal (length lx) 3)(setf i-node (find-move (read-from-string (elt lx 0)) (elt lx 1) (elt lx 2))))
    (if (equal i-node -1)(return-from get-segment-group nil))
    (setf nd (elt *node* i-node))
    (dotimes (i (length (getf nd :set-seed)))
       (setf lx (get-segment-forward (elt (getf nd :set-seed) i) num1 bw1 mv1 num2 bw2 mv2))
       (if (not (equal lx nil))
	   (progn
             (setf b1 (cmp-segments segment lx))
	     (if (equal b1 T)(setf ls (append ls (list (elt (getf nd :set-seed) i)))))
	     ))
       )
    (return-from get-segment-group ls)
    )
  )

(defun eval-nextmove-nn (segment)
  (let ((ls-games nil)(ls-moves nil)(lx nil)(lsn nil)(lr nil)(num1 nil)(bw1 nil)(mv1 nil)(num2 nil)(bw2 nil)(mv2 nil)(num3 nil)(bw3 nil)(mv3 nil)(b1 nil))
    (setf ls-games (get-segment-group segment))
    (if (equal ls-games nil)(return-from eval-nextmove-nn nil))

    (setf mv2 mv2)
    (setf lx (tl-split '(#\|) (elt segment 0)))
    (if (equal (length lx) 3)
	(progn
          (setf num1 (read-from-string (elt lx 0)))
	  (setf bw1 (elt lx 1))
	  (setf mv1 (elt lx 2))
	  ))

    (setf lx (tl-split '(#\|) (elt segment (- (length segment) 1))))
    (if (equal (length lx) 3)
	(progn
          (setf num2 (read-from-string (elt lx 0)))
	  (setf bw2 (elt lx 1))
	  (setf mv2 (elt lx 2))
	  ))
    (setf mv3 "?")
    (if (equal bw2 "B")
	(progn
          (setf num3 (+ num2 1))
	  (setf bw3 "W")
	  ))
    (if (equal bw2 "W")
	(progn
          (setf num3 num2)
	  (setf bw3 "B")
	  ))
    (dolist (w ls-games)
      (setf ls-moves (get-segment-forward w num1 bw1 mv1 num3 bw3 mv3))
      (if (not (equal ls-moves nil))
	  (progn
	    (setf b1 nil)
            (dolist (mv ls-moves)
              (setf lx (tl-split '(#\|) mv))
	      (if (and (equal num3 (read-from-string (elt lx 0))) (equal bw3 (elt lx 1)))(setf b1 T))
	      (if (equal b1 T)
		  (progn
		    (setf lr (cons w mv))
		    ;(print lr)
		    (push lr lsn)
                    ;(setf lsn (append lsn lr))
		    (return)
		    ))
	      )
	    ))
      )
    (return-from eval-nextmove-nn lsn)
    )
  )

;on reçoit de eval-nextmove-nn une liste de cons ou le car est le seed du game et le cdr est le move
;on produit une liste de listes de type move (ou any *|*|*) suivi par W B N (en nombre)
(defun get-mv-stats (ls)
  (let ((a "*|*|*")(nbw 0)(nbb 0)(nbn 0)(s1 nil)(s2 nil)(lmv nil)(lz nil)(b1 nil)(bw nil)(lx nil))
    (dolist (w ls)
      (setf s1 (subseq (car w) (- (length (car w)) 1)))
      (if (equal s1 "W")(setf nbw (+ nbw 1)))
      (if (equal s1 "B")(setf nbb (+ nbb 1)))
      (if (equal s1 "N")(setf nbn (+ nbn 1)))
      (setf s2 (cdr w))
      (setf lx (tl-split '(#\|) s2))
      (setf bw (elt lx 1))
      (setf a (concatenate 'string "*|" bw  "|*"))
      (setf b1 nil)
      (dolist (m lmv)
        (if (equal s2 m)(progn
                          (setf b1 T)
			  (return)
			  ))
	)
      (if (equal b1 nil)(push s2 lmv))
      )

    (push (list a nbw nbb nbn) lz)
    ;(print lz)
    (dolist (m lmv)
      (setf nbw 0)
      (setf nbb 0)
      (setf nbn 0)
      (dolist (w ls)
        (if (equal m (cdr w))(progn
                               (setf s1 (subseq (car w) (- (length (car w)) 1)))
                               (if (equal s1 "W")(setf nbw (+ nbw 1)))
                               (if (equal s1 "B")(setf nbb (+ nbb 1)))
                               (if (equal s1 "N")(setf nbn (+ nbn 1)))
			       ))
	)
      (push (list m nbw nbb nbn) lz)
      )
    (return-from get-mv-stats (reverse lz))
    )
  )

;on reçoit la liste de get-mv-stats et on calcule les probas pour le camp
;considéré W ou B prob de gagner et proba de ne pas perdre pour all et chaque coup
(defun get-mv-pct (ls)
  (let ((nbw 0)(nbb 0)(nbn 0)(m nil)(pwin 0)(pnotlose 0)(tot 0)(lz nil)(lx nil)(bw nil))
    (dolist (w ls)
      (setf m (elt w 0))
      (setf lx (tl-split '(#\|) m))
      (setf bw (elt lx 1))
      (setf nbw (elt w 1))
      (setf nbb (elt w 2))
      (setf nbn (elt w 3))
      (setf tot (+ nbw nbb nbn))
      (if (equal bw "W")
	  (progn
	    (setf pwin (float (/ (* nbw 100) tot)))
	    (setf pnotlose (float (/ (* (+ nbw nbn) 100) tot)))
	  ))
      (if (equal bw "B")
	  (progn
	    (setf pwin (float (/ (* nbb 100) tot)))
	    (setf pnotlose (float (/ (* (+ nbb nbn) 100) tot)))	    
	  ))
      (push (list m pwin pnotlose) lz)
      )
    (return-from get-mv-pct lz)
    )
  )

;on re&oit aussi la liste de get-mv-stats dans le but de proposer le 
;meilleur move (best not to loose) dont le total games soit > total average games per move of the list
(defun get-mv-ntl1 (ls)
  (let ((m nil)(nbw 0)(nbb 0)(nbn 0)(av 0)(tot 0)(lz nil)(lzb nil)(zb nil)(pnotlose 0)(k 0)(lx nil))
    (dolist (w ls)
      (setf nbw (elt w 1))
      (setf nbb (elt w 2))
      (setf nbn (elt w 3))
      (setf tot (+ tot nbw nbb nbn))
      )
    (setf av (float (/ tot (length ls))))
    ;(print av)
    (setf lz (get-mv-pct ls))
    (loop
       (when (equal (length lz) 0) (return))
       (setf pnotlose -1)
       ;(print lz)
       (dolist (z lz)
         (if (> (elt z 2) pnotlose)(progn
                                     (setf zb z)
				     (setf pnotlose (elt z 2))
				     ))
	 )
       (setf lx (tl-split '(#\|) (elt zb 0)))
       (if (not (equal (elt lx 0) "*"))(push zb lzb))
       (setf lz (remove zb lz))
       (setf k (+ k 1))
       (if (> k 150)(return-from get-mv-ntl1 "trouble"))
       )
    (setf lzb (reverse lzb))
    ;(print lzb)
    (setf zb nil)
    (dolist (b lzb)
      (dolist (w ls)
	(setf m (elt w 0))
        (setf nbw (elt w 1))
        (setf nbb (elt w 2))
        (setf nbn (elt w 3))
        (setf tot (+ nbw nbb nbn))
	(if (equal m (elt b 0))(return))
	)
      (if (> tot av)(progn
                      (setf zb b)
		      (return)
		      ))
      )
    (return-from get-mv-ntl1 zb)
    )
  )

