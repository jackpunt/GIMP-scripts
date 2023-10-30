(gimp-message "loading debug-script")

(define atm (vector-ref TILE-DECK 2))
(define tile7 (vector-ref TILE-DECK 7))
(define extras1 (list-tail tile7 8))
(define extras2 '((vp "VP" (size 50) (lead 18) (left -8))
		  (cardProps (rent 0) (vp 0) (onStep (when (isOwner #t) (dist (add -2))) (else (dist (add -1)))))
		  (line 305 BLACK 35 4)
		  (text "Cost" 30 328 LEFTJ 35 TEXTFONT BLACK)
		  (text "2\n5\n8\n11\n14" 80 374 RIGHTJ 40 TEXTFONT BLACK)
		  (text "House . . . . . .\nTriplex . . . . .\nApartment .\nHigh Rise . .\nTower . . . . . ." 110 374 LEFTJ 40 TEXTFONT BLACK)
		  (text "VP" 372 328 RIGHTJ 40 TEXTFONT BLACK)
		  (text "1\n3\n6\n10\n15" 370 374 RIGHTJ 40 TEXTFONT BLACK)
		  (text "*Rent" -115 328 LEFTJ 40 TEXTFONT BLACK)
		  (text ".5\n1\n2\n4\n8" -60 374 CENTER 40 TEXTFONT BLACK)
		  ))

(define p3 (vector-ref POLICY-DECK 3))
(define (pn n) (card-make-one-card (vector-ref POLICY-DECK n)))
(define (cwi spec)
  (let* ((title (nth 3 spec)) 
	 (extras (list-tail spec 7)))
    (card-write-info "policy" (syms-to-alist title extras))
    ))

(define (acte card-spec)
  (let* ((nreps (abs (nth 0 card-spec))) ; (< (nth 0 card-spec) 0) for non-templated 'cards'
	 (type (nth 1 card-spec)) (types (type-string type))
	 (name (nth 2 card-spec))
	 (args (list-tail card-spec 3))     ; (nthcdr 3 card-spec) == (rest)
	 )
    (apply card-type-event nreps types name args)))

(define h3 (vector-ref HOME-DECK 3))
(define (acth spec)
  (let* ((nreps (abs (nth 0 spec)))	; (< (nth 0 card-spec) 0) for non-templated 'cards'
	 (type (nth 1 spec)) (types (type-string type))
	 (name (nth 2 spec))
	 (args (list-tail spec 3))	; (nthcdr 3 card-spec) == (rest)
	 )
    (apply card-type-home nreps types name BROWN args))) ; color is evaluated...


(define (raws color spec)
  (let* ((title (nth 3 spec))
	 (extras (list-tail spec 7))
	 (filen title))
    (macro-expand `(raw-specs '(title color extras)))))

(define debug-script #t)
(gimp-message "debug-script loaded")
