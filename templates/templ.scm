(require "templates/script-fu-para-cards.scm" 'file-merge-and-save)

(define PPG-POKER-18-SPEC '((file "PPGPoker18-0.png") (cardw 1108) (cardh 808)
			    (xmin 120) (ymin 85) (xinc 1125) (yinc 825)
			    (ncol 3) (nrow 6) (edge 50) (radi 62)))
(define MY-POKER-8-SPEC '((file "MyPoker8-0.png") (cardw 1108) (cardh 808)
			  (xmin 11) (ymin 85) (xinc 1125) (yinc 825)
			  (ncol 2) (nrow 4) (edge 50) (radi 62)))
(define PPG-MINI-36-spec '((file "PPGMiniCard36-0.png") (cardw 800) (cardh 575)
			   (xmin 150) (ymin 100) (xinc 833) (yinc 578.25)
			   (edge 50) (radi 62) (xlim 3600) (ylim 5400)))
(define MY-MINI-18-SPEC '((file "MyMiniCard18-0.png") (cardw 750) (cardh 525)
			  (xmin 84) (ymin 25) (xinc 752) (yinc 527)
			  (ncol 3) (ncol 6) (edge 25) (radi 37)))
(define MY-MICRO-18-SPEC '((file "MyMiniCard18-0.png") (cardw 250) (cardh 175)
			  (xmin 84) (ymin 25) (xinc 752) (yinc 527)
			  (ncol 3) (ncol 6) (edge 25) (radi 37))) ; dubious
(define MY-TOKEN-24-SPEC '((file "MyTokenSpec-0.png") (cardw 115) (cardh 115)
			   (xmin 50) (ymin 50) (xinc 117) (yinc 117)
			   (edge 1) (radi 1) (xlim 750) (ylim 525))) 

;;; choose with (template-use 'project-dir' SOME-SPEC)
;; spec: (file "basename.png") (cardw ) (cardh ) (xmin ) (ymin ) (xinc )(yinc ) (xlim ) (ylim )
;;  (card-edge )(corner-radius ) [ncards?] (ncol ) (nrow )

(define BASE-DIR "/Users/jpeck/Google Drive/jpeckj/")

;;;; not yet object-ified:
(define PROJECT-DIR "")
(define IMAGE-DIR BASE-DIR)
(define CARD-DIR BASE-DIR)
(define PUB-DIR  BASE-DIR)
(define TEMPLATE-DIR BASE-DIR)

(define (set-template-dirs proj)
  (set! PROJECT-DIR  proj)
  (set! IMAGE-DIR    (string-append BASE-DIR proj "/images/"))
  (set! CARD-DIR     (string-append BASE-DIR proj "/cards/"))
  (set! PUB-DIR      (string-append BASE-DIR proj "/publish/"))
  (set! TEMPLATE-DIR (string-append BASE-DIR proj "/templates/")))

(define temp #f)
;;;; Entry-point:
(define (template-use proj spec)
  (set-template-dirs proj)
  (set! temp (Templ spec)))


;;;; helpers:
;; size/orientation of a given CARD:
(define (card-width image)  (car (gimp-image-width image)))
(define (card-height image) (car (gimp-image-height image)))
(define (starts-with? str prefix) (string-prefix? prefix str))

(define (get-alpha layer x y)
  (let* ((pxi (gimp-drawable-get-pixel layer x y))
	 (ndim (car pxi))
	 (pxary (cadr pxi)))
    (if (> ndim 3) (vector-ref pxary 3) -1)))

;; (define tt (Templ spec))
;;
;; spec: (filename "path") (cardw ) (cardh ) (xmin ) (ymin ) (xinc )(yinc ) (xlim ) (ylim )
;;  (card-edge )(corner-radius ) [ncards?] (ncol ) (nrow )

;;;; EXPORTS: card-to-template, save-if-template-full
(macro (def-slot form)
  (let ((name (cadr form))
	(rest (cddr form)))
  `(define ,name (util-assq (quote ,name) tspec ,@rest))))


(define (Templ tspec)
  (make-environment
    ;; initialize 'fields'
    (define spec tspec)			  ; original spec [or should we (apply list spec) ?
    (define file (util-assq 'file tspec)) ; basename.png
    (define filepath (string-append TEMPLATE-DIR file))
    (define cardw (util-assq 'cardw tspec 750))	; no bleed
    (define cardh (util-assq 'cardh tspec 525))	; no bleed
    (define ncol (util-assq 'ncol tspec 3))
    (define nrow (util-assq 'nrow tspec 6))
    (define xmin (util-assq 'xmin tspec 84))	; printer margin
    (define ymin (util-assq 'ymin tspec 25))	; printer margin
    (define xinc (util-assq 'xinc tspec cardw))	; 
    (define yinc (util-assq 'yinc tspec cardh))	; 
    (define xlim (util-assq 'xlim tspec (+ xmin (* ncol xinc))))
    (define ylim (util-assq 'ylim tspec (+ ymin (* nrow yinc))))
    (define origx (util-assq 'origx tspec 0))
    (define origy (util-assq 'origy tspec 0))
    (define tempw (util-assq 'xinc tspec xlim))
    (define temph (util-assq 'xinc tspec ylim))

    (define card-edge (util-assq 'card-edge tspec 25))	 ; safe
    (define corner-radius (util-assq 'corner-radius tspec 37)) ; ~ 1/8th inch, 62 ~ 1/5 inch

    (define is-portrait? (< cardw cardh)) ; template-orientation

    ;; define 'methods'
    (define (set-ilxy ilxy . lxy)
      (set! LAST-ILXY (if (pair? ilxy) ilxy `(,ilxy ,@lxy))))

    (define (set-end-of-template)
      ;; mark LAST-ILXY as INVALID; with xmin, ymin for next template image
      ;; force open-ilxy-file
      (set-ilxy `(-1 -1 ,xmin ,ymin)))
    (define LAST-ILXY (set-end-of-template))
    
    (define msg-ctt #f)
    ;; put-card
    (define (card-to-template image layer undo context)
      ;; image to next open slot, updating LAST-ILXY
      (and msg-ctt (message-string1 "ctt-0:" image layer undo context))
      (set-ilxy (get-empty-ilxy))		 ; possibly a new image-template?
      ;; assert LAST-ILXY is valid image & layer
      (and undo (gimp-image-undo-group-start image)) ; mark for undo
      (card-to-this-ilxy image LAST-ILXY context)    ;
      (save-if-template-full)		; TRY THIS! eager save when template is full
      (and undo (gimp-image-undo-group-end image))
      (and msg-ctt (message-string "ctt-3: LAST-IXY" LAST-ILXY))
      )

    (define (get-empty-ilxy)
      (when (< (car LAST-ILXY) 0)
	(and msg-ctt (message-string "ctt-1: OPEN TEMPLATE:" file))
	(set-ilxy (open-ilxy-file filepath)))
      (next-ilxy-empty LAST-ILXY 40)
      )
      
    ;; return list (image layer x y) of corner of a new slot on template-image
    ;; assert (image layer) is valid.
    (define (next-ilxy-empty ilxy count)
      (if (< count 1) (throw "Looping:" ilxy))      
      (if (is-card-at ilxy) 
	  (next-ilxy-empty (apply next-ilxy ilxy) (- count 1))
	ilxy))

    ;; return #f if pixel at image[lx,ly] is transparent (or off template)
    (define (is-card-at image layer lx ly)
      ;; catch & return #t if [lx,ly] is outside of image:
      (let* ((cx (+ lx origx))
	     (cy (+ ly origx))
	     (xy (rotated-xy #f cx cy xoff yoff))
	     (rx (nth 0 xy))
	     (ry (nth 1 xy))
	     (alpha (catch 222 (get-alpha layer rx ry))))
	(message-string "is-card-at:" lx ly "-->" cx cy "-->" rx ry "==>" alpha)
	(> alpha 0)))

    (define (next-ilxy image layer x y)
      ;; return (image layer x y) of next template slot (maybe new image)
      (let* ((full-ilxy (is-template-full? image layer x y))
	     ;;(msg1 (message-string "next-ilxy: full-ilxy=" full-ilxy))
	     (full? (nth 0 full-ilxy))
	     (rv-ilxy (if full?
			  ;; open new (image layer xmin ymin) [or BACKFILL partial template]
			  (open-ilxy-file #f)
			(nth 1 full-ilxy) ; ILXY of NEXT slot
			)))
	;;(message-string "next-ilxy: full?" full? "rv=" rv-ilxy)
	rv-ilxy
	))

    ;; return (full? ilxy) if NOT full?, then ilxy is next slot on template-image
    ;; That is: get-next-slot AND is-next-slot-on-template?
    (define (is-template-full? image layer x y)
      ;; Template is FULL if LAST (x,y) indicates the *NEXT* slot is off-template.
      ;; return (full? next-x next-y)

      ;;(message-string "is-template-full?:" image layer x y (xinc) (yinc) (xmin))
      (set! x (+ x (xinc))) 		; advance to next slot
      (let* ((rxy (rotated-xy #f x y cardw cardh))
	     (rx (nth 0 rxy))
	     (ry (nth 1 rxy)))
	;;(message-string "is-template-full?" rx ry)
	(when (> rx tempw)
	  (set! x xmin)
	  (set! y (+ y yinc))
	  (set! ry (+ ry yinc)))
	;;(message-string "is-template-full?" (> ry (temph)) x y )
	(list (> ry temph) (list image layer x y)))
      )
    ;; lx, ly are from ILXY; dx, dy identify the 'sample point' of interest
    (define (rotated-xy portrait? lx ly dx dy) ; dxy: (xoff)(yoff)
      ;; return (x y) on template equiv to (lx ly) on un-rotated card.
      (let* ((sp? portrait?)		; src is portrait? mode
	     (tp? is-portrait)		; template-slot is portrait?
	     (norot? (eq? sp? tp?))	; src is not rotated on template...?
	     (rx (+ lx (if norot? dx (- cardh dy))))
	     (ry (+ ly (if norot? dy dx))))
	(list rx ry)))

    (define msg-open-ilxy #f)

    ;; backfill existing open-ilxy template OR open a new template from filename
    ;; file: filename OR #t to increment name from current template-image
    (define (open-ilxy-file file)
      (and msg-open-ilxy (message-string "open-ilxy-file:" file "open-ilxy =" open-ilxy ))
      (or (and (not file) use-backfill (backfill-ilxy) )
	  ;; get ilxy of a NEW template image
	  (let* ((image (next-template-file-image file))		     ; -N+1.png
		 (layer (image-base-layer image))
		 (ilxy (list image layer xmin ymin))
		 )
	    (if (not xlim) (set! xlim (car (gimp-image-width image))))
	    (if (not ylim) (set! ylim (car (gimp-image-height image))))
	    (set-open-ilxy (cons ilxy (get-open-ilxy))) ; shift onto open-ilxy
	    (and msg-open-ilxy (message-string "open-ilxy-file2" file "ilxy:" ilxy "LAST-ILXY" LAST-ILXY))
	    ilxy
	    )))

    ;; holding the FIRST-ILXY for each image (image layer (xmin) (ymin))
    (define open-ilxy '())     
    (define (get-open-ilxy) open-ilxy)
    (define (set-open-ilxy ilxy-list) (set! open-ilxy ilxy-list))

    (define use-backfill #t)
    (define (set-use-backfill val) (set! use-backfill val))

    ;; return ILXY or #f
    (define (backfill-ilxy)
      (define (prune-open ilxy)
	(let ((open? (catch #f (next-empty-ilxy ilxy))))   ; #f if image is invalid/deleted
	  (if (not open?) (set-open-ilxy (cdr open-ilxy))) ; unshift when elt is not usable
	  open?))

      (let ((ilxy (list-search-positive open-ilxy prune-open))) ; next-empty-ilxy
	(and msg-open-ilxy (message-string "backfill-ilxy" ilxy "open-ilxy" open-ilxy))
	ilxy))

    (define last-template-filename #f)
    ;; make a new, empty template image (&layer)
    ;; when invoked with a filename, it is likely filename-0.png [the zeroth/original template file]
    ;; if filename == #f, then proceed to *next* incremental filename [max++]
    (define (next-template-file-image file)	; .../dir2/dir1/card-name-N.png or #t
      ;; Extract -N.png and use -N+1.png
      ;; file name MUST end with "-N" (this image/file now "filled" with cards)
      ;; find/open version named "-0" [template-0.png, MyMiniCards18-0.png]
    
      ;; protection during debug
      (define (quit-on-limit n lim) (if (> n lim) (throw (stringifyf "quit-on-limit" n ">" lim))))

      (let* ((filename (or file last-template-filename))
	     (rslash (reverse (strbreakup filename DIR-SEPARATOR))) ; card-name-N.png dir1 dir2 ...
	     (name-dot-type (strbreakup (car rslash) "."))	    ; (card-name-N png)
	     (dot-type (string-append "." (nth 1 name-dot-type)))   ; .png
	     (name-num (strbreakup (car name-dot-type) "-"))	    ; (card name N)
	     (num-names (reverse name-num))			    ; (N name card)
	     (basename (unbreakupstr (reverse (cdr num-names)) "-")) ; card-name
	     (new-num (+ 1 (string->number (car num-names))))	     ; N+1
	     (num-str (number->string new-num))			     ;"N+1"
	     ;;(xxx (quit-on-limit new-num 10))
	     (oname (string-append basename "-" num-str dot-type)) ; cards-N+1.png
	     (cname (string-append TEMPLATE-DIR "/" oname ))	   ; TEMPLATE-DIR/cards-N+1.png
	   
	     ;; open ${basename}-0.${type} and set next filename:
	     (name-0 (string-append basename "-0" dot-type)) ; card-name-0.png
	     (temslash (reverse (cons name-0 (cdr rslash)))) ; (... dir2 dir1 card-name-0.png)
	     (template (unbreakupstr temslash DIR-SEPARATOR)) ;  .../dir2/dir1/card-name-0.png
	     (image (car (gimp-file-load RUN-NONINTERACTIVE template oname)))
	     (layer (image-base-layer image))
	     (rename (gimp-image-set-filename image cname))
	     (display (car (gimp-display-new image))) ; which display/tab shows image
	     )
	(set! last-template-filename template) ; save new filename
	(para-set-comment image oname)	       ; the short name
	(para-set-display image display)       ; record Display
	;;(message-string "next-template-file-image:" image layer display cname)
	image))

    (define msg-save-if-full #f)
    (define (set-msg-save-if-full val) (set! msg-save-if-full val))

    ;; either repeated (card-to-template...) OR explicit 'flush' by caller, with ((car alt) = #t)
    ;; return (car (is-template-full? LAST-ILXY)) [Mar 2022]
    (define (save-if-template-full . arg)
      ;; Save template associated with LAST-ILXY [exported API]
      ;; save IFF (XOR full? alt?)
      (and msg-save-if-full (message-string "save-if-template-full0: arg=" agr LAST-ILXY))
      (if (equal? TRUE (car (gimp-image-is-valid (car LAST-ILXY)))) ; ie: NOT immediately after (set-end-of-template)
	  (let ((full? (car (apply is-template-full? LAST-ILXY)))
		(alt? (if (pair? arg) (car arg) #f))) ; (util-opt-arg arg #f)
	    (and msg-save-if-full (message-string "save-if-template-full1: " full? alt? arg))
	    (if (or (and full? (not alt?)) (and alt? (not full?)))
		(save-template LAST-ILXY) ; returns cname
	      )
	    )))

    ;; set to #f to prevent writing of any template
    (define save-template-enabled #t) 
    (define (enable-save-template value) (set! save-template-enabled))

    (define (save-template ilxy)
      ;; save given image (now ~full of cards) to filename-N.png:
      ;; return complete name of the saved file
      ;;(message-string "save-template" (template-get-spec))
      (when save-template-enabled
	(let* ((image (nth 0 ilxy))
	       (layer (nth 1 ilxy))
	       (cname (car (gimp-image-get-filename image))) ; TEMPLATE-DIR/template-name-N.png
	       (oname (util-file-basename cname))	     ; template-name-N.png
	       (pname (or (para-get-global-pubname) oname)) ; "Homes" or #f; set by card-set-page-name: (0 page "Name")
	       (sname (rename-to-dir PUB-DIR oname pname))  ; PUB-DIR/pname-N.png
	       (xname (rename-to-dir TEMPLATE-DIR oname pname "xcf"))
	       (mode RUN-NONINTERACTIVE))
	  (message-string "save-template publish:" sname oname)
	  (para-set-comment image pname)
	  (file-png-save-defaults mode image layer sname oname) ; save .PNG in PUB-DIR
	  (gimp-xcf-save          mode image layer xname oname) ; save .XCF in TEMPLATE-DIR
	  cname
	  )))

    ))

(message-string "templ loaded")
