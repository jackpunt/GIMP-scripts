;;; This directory in the GIMP Preferences for 'scripts'
;;; /Users/jpeck/Google Drive/jpeckj/GIMP/2.10/scripts
(require "templates/script-fu-para-cards.scm" 'file-merge-and-save)

;;; Exports: templ::exports, card-to-template, use-template 

(define BASE-DIR "/Users/jpeck/Google Drive/jpeckj/")

;;; TEMPLATE (width height) [generally: the card in LANDSCAPE]
;;;                          TEMPLATE-FILE TEMPLATE-CARD-SIZE TEMPLATE-MIN-OFFSET-INC
(define MY-POKER-8-SPEC   '("MyPoker8-0.png"     (1108 808)  11 85 1125 825.00 50 62)) ; full-bleed
(define PPG-POKER-18-SPEC '("PPGPoker18-0.png"   (1108 808) 120 85 1125 825.00 50 62)) ; [GtR_new] (233 190)
(define PPG-MINI-36-SPEC  '("PPGMiniCard36-0.png" (800 575) 150 100 833 578.25 50 62)) ; 3600x5400 -> 36 Cards 
(define MY-MINI-18-SPEC   '("MyMiniCard18-0.png"  (750 525)  84 25  752 527 25 37)) ; no bleed
(define MY-MICRO-18-SPEC  '("MyMiniCard18-0.png"  (250 175)  84 25  752 527 25 37)) ; no bleed
;; 115 * 6 = 690 < 750, 115*4 = 460 < 525
;; 117 * 6 = 702 < 750, 117*4 = 468 < 525
(define MY-TOKEN-24-SPEC  `("MyTokenSpec-0.png"   (115 115)  24  14 117 117  1  1)) ; inside MINI card


;;; choose with (template-use 'project-dir' SOME-SPEC)

(define TEMPLATE MY-MINI-18-SPEC)
(define TEMPLATE-FILE (car MY-MINI-18-SPEC))
(define TEMPLATE-CARD-SIZE (apply list (cadr MY-MINI-18-SPEC))) ; copy just to be safe
(define TEMPLATE-MIN-OFFSET-INC (cddr MY-MINI-18-SPEC))
(define TEMPLATE-ORIG-X 0)
(define TEMPLATE-ORIG-Y 0)
(define TEMPLATE-XMIN (nth 0 TEMPLATE-MIN-OFFSET-INC))
(define TEMPLATE-YMIN (nth 1 TEMPLATE-MIN-OFFSET-INC))
;; (xoff) -- sample location for get-alpha: (nth 2)
;; (yoff) -- sample location for get-alpha: (nth 3)
(define TEMPLATE-XINC (nth 2 TEMPLATE-MIN-OFFSET-INC))
(define TEMPLATE-YINC (nth 3 TEMPLATE-MIN-OFFSET-INC))
(define TEMPLATE-EDGE (nth 4 TEMPLATE-MIN-OFFSET-INC))
(define TEMPLATE-RADI (nth 5 TEMPLATE-MIN-OFFSET-INC))

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

(define (set-template-spec spec)
  (set! TEMPLATE spec)
  (set! TEMPLATE-FILE (car spec))
  (set! TEMPLATE-CARD-SIZE (apply list (cadr spec))) ; copy just to be safe
  (set! TEMPLATE-MIN-OFFSET-INC (apply list (cddr spec)))
  (set! TEMPLATE-XMIN (nth 0 TEMPLATE-MIN-OFFSET-INC))
  (set! TEMPLATE-YMIN (nth 1 TEMPLATE-MIN-OFFSET-INC))
  (set! TEMPLATE-EDGE (nth 4 TEMPLATE-MIN-OFFSET-INC))
  (set! TEMPLATE-RADI (nth 5 TEMPLATE-MIN-OFFSET-INC))
  )

;;; User/Caller use this to configure TEMPLATE
(define (template-use proj spec)
  (set-template-dirs proj)
  (set-template-spec spec))

(define (get-template) TEMPLATE)

;;(template-use "GtR_new" PPG-POKER-18-SPEC)
;;(template-use "Estates" MY-MINI-18-SPEC)

(define (template-get-spec)
  (list TEMPLATE-DIR TEMPLATE-FILE TEMPLATE-CARD-SIZE TEMPLATE-MIN-OFFSET-INC))

(define (template-orientation)
  ;; true if portrait mode
  ;;(< (nth 4 TEMPLATE-MIN-OFFSET-INC) (nth 5 TEMPLATE-MIN-OFFSET-INC)))
  (< TEMPLATE-XINC TEMPLATE-YINC))
;;

(define TEMPLATE-EMPTY (nth 0 '((255 255 255) (0 0 0)))) ; 0=WHITE 1=BLACK transparent
;;; GTR used WHITE, EST uses BLACK [(which-empty) now checks each template/file]
;;; NOTE: with gimp-image-pick-color it does not matter if the area is transparent or solid!
;;; can use solid WHITE to mask out a slot for refill
;;; SO: the "is-card-at" location must not be suseptible to solid WHITE/BLACK
;;; we use hueristic of looking at pixel[1,1] to determine usage pattern for template

;;; use new -0 template file:
(define LAST-ILXY `(-1 -1 ,TEMPLATE-XMIN ,TEMPLATE-YMIN))

(define (set-end-of-template)
  ;; mark LAST-ILXY as INVALID; witn xmin, ymin for next template image
  ;;(message-string "set-end-of-template" LAST-ILXY TEMPLATE-CARD-SIZE)
  (set-ilxy `(-1 -1 ,TEMPLATE-XMIN ,TEMPLATE-YMIN)))

(define (set-ilxy ilxy . lxy)
  ;; update/store LAST-ILXY
  ;;(message-string "set-ilxy" ilxy)
  (if (pair? ilxy)
      (set! LAST-ILXY ilxy)
    (set! LAST-ILXY `(,ilxy ,@lxy))))

(define (script-fu-template-n image drawable nreps)
  ;;(message-string "N to template: " nreps)
  (while (> nreps 0)
	 (card-to-template image drawable #t #t)
	 (set! nreps (- nreps 1)))
  )
	   
(define (script-fu-template image drawable)
  (card-to-template image drawable #t #t)
  )
(define (script-fu-template-debug image drawable)
  (card-to-template image drawable #f #t)
  )

(define (starts-with? str prefix) (string-prefix? prefix str))

;; See what TRANSPARENT looks like on this image:
(define (which-empty image)
    ;; See what TRANSPARENT or EMPTY looks like on this image:
  (let* ((layer (car (gimp-image-get-active-layer image)))
	 (pxi (gimp-drawable-get-pixel layer 1 1)) ; presumed empty pixel
	 (ndim (car pxi))
	 (pxary (cadr pxi)) ; array
	 (empty (vector-ref pxary 1)) ; rGb
	 (empty (if (and (> ndim 3) (= (vector-ref pxary 3) 0))
		    (list empty empty empty)
		    '(255 255 255))))	; default to WHITE
    (set! TEMPLATE-EMPTY empty)
    ;;(message-string "TEMPLATE-EMPTY=" TEMPLATE-EMPTY)
    ))

(define (get-alpha image layer x y)
  (let* ((pxi (gimp-drawable-get-pixel layer x y))
	 (ndim (car pxi))
	 (pxary (cadr pxi)))
    (if (> ndim 3) (vector-ref pxary 3) -1)))


;;; From MENU/GUI: (re-) start using the current template image:
;;; NOTE: do NOT use basename-0 (each NEW page copies FROM basename-0)
(define (use-template image layer)
  (set! DO-TEMPLATE #t)
  (which-empty image)
  (let* ((filename (car (gimp-image-get-filename image)))
	 (basename (util-file-basename filename))
	 (base-num (car (reverse (strbreakup basename "-")))) ; file-name-N.png -> (N.png name file)
	 )
    (message-string "use-template" image layer basename base-num)
    (if (starts-with? base-num "0.")
	;; force to new/next filename (do not overwrite template-0.png)
	(set-ilxy (next-image-layer-xy -1 -1 filename)) ; now ILXY is valid (start of empty template)
	;; otherwise: just search for first/next empty slot:
	(let ((x TEMPLATE-XMIN)(y TEMPLATE-YMIN))
	  (set-ilxy (list image layer x y)) ; first slot of this image
	  (set-ilxy (next-ilxy-empty))))    ; first empty slot of template
    (message-string "use-template:" LAST-ILXY (gimp-image-get-filename (car LAST-ILXY)) "empty=" TEMPLATE-EMPTY )
    ))

(define msg-ctt #t)
(define (get-empty-ilxy)
  (when (< (car LAST-ILXY) 0)
    (let ((filename (string-append TEMPLATE-DIR TEMPLATE-FILE)))
      (and msg-ctt (message-string "ctt-1: OPEN TEMPLATE:" TEMPLATE-FILE))
      (set-ilxy (next-image-layer-xy -1 -1 filename))))
  (let ((real-ilxy (next-ilxy-empty)))
    (and msg-ctt (message-string "ctt-2: empty-ixly=" real-ilxy))
    real-ilxy)
  )

(define (card-to-template image layer undo context)
  ;; image to next open slot, updating LAST-ILXY
  (and msg-ctt (message-string1 "ctt-0:" image layer undo context))
  (set-ilxy (get-empty-ilxy))	     ; possibly a new image-template?
  ;; assert LAST-ILXY is valid image & layer
  (and undo (gimp-image-undo-group-start image)) ; mark for undo
  (card-to-this-ilxy image LAST-ILXY context)	 ;
  (save-if-template-full)			 ; TRY THIS! eager save when template is full
  (and undo (gimp-image-undo-group-end image))
  (and msg-ctt (message-string "ctt-3: LAST-IXY" LAST-ILXY))
  )


(define (card-to-this-ilxy image ilxy context)
  ;; rotate/move image to ilxy & card-parasite-record-image
  (let* ((layer 0)			; src layer (not used)
	 (srcW (car (gimp-image-width image)))
	 (srcH (car (gimp-image-height image)))
	 (destImage (nth 0 ilxy))	; implied by destLayer
	 (destLayer (nth 1 ilxy))
	 (destX (+ (nth 2 ilxy) TEMPLATE-ORIG-X))
	 (destY (+ (nth 3 ilxy) TEMPLATE-ORIG-Y)))
    ;;(message-string "ctt-ilxy: ILXY=" ilxy "(image layer)" (list image layer))

    ;; merge & clip
    (set! layer (car (gimp-image-merge-visible-layers image CLIP-TO-IMAGE)))
    (gimp-selection-all image)		; select ALL
    (gimp-edit-copy layer)		; copy selection to edit-buffer [layer not used]
    ;; return should be TRUE [FALSE iff nothing to copy]
    (and context (gimp-context-push))
    (and context (gimp-context-set-defaults))

    (gimp-context-set-transform-resize TRANSFORM-RESIZE-ADJUST)
    (let* ((rotImage (car (gimp-edit-paste-as-new-image))) ; edit-paste, [rotate], edit-copy
	   (rotLayer (car (gimp-image-get-active-layer rotImage)))
	   (floatLayer 0))
      (maybeRotate rotLayer (< srcW srcH) (template-orientation))
      (gimp-edit-copy rotLayer)				       ; rotated image to edit-buffer
      (set! floatLayer (car (gimp-edit-paste destLayer TRUE))) ; paste to template
      ;;(gimp-drawable-offset floatLayer FALSE OFFSET-TRANSPARENT destX destY)
      (gimp-layer-set-offsets floatLayer destX destY)
      (gimp-floating-sel-anchor floatLayer)
      (gimp-image-delete rotImage)
      (gimp-displays-flush)
      ;; Record cross-links in parasites
      (card-parasite-record-image image destImage destLayer destX destY)
      )
    (and context (gimp-context-pop))
    (gimp-displays-flush)
    )
  )

(define (maybeRotate layer os od)
  ;; if orientations differ, then rotate 90-degrees:
  ;; TODO: scale to match (tempw, temph)
  (if (or (and os (not od)) (and od (not os)))
      (gimp-item-transform-rotate-simple layer ROTATE-90 FALSE 0 0)))

;;; deck is Array[Array[N front-file back-file]]
(define (make-deck deck)
  (let ((dir "/Google Drive/jpeckj/GtR_new/clean/"))
    (let loop ((ndx 0))
      )
    )
  )

;;(define (get-entrypoints) 
(define templ
  (make-environment 
    ;; template paramters:
    (define (xmin) TEMPLATE-XMIN)	; loc of left edge of cards on template
    (define (ymin) TEMPLATE-YMIN)	; loc of top row of cards on template

    ;; Card parameters
    (define (cardw) (nth 0 TEMPLATE-CARD-SIZE))   ; width of card on template
    (define (cardh) (nth 1 TEMPLATE-CARD-SIZE))   ; height of card on teamplate
    (define (xinc) (nth 2 TEMPLATE-MIN-OFFSET-INC)) ; slightly greater than cardw
    (define (yinc) (nth 3 TEMPLATE-MIN-OFFSET-INC)) ; slightly greater than cardh

    (define (xoff) (nth 5 TEMPLATE-MIN-OFFSET-INC)) ; (xoff,yoff) where color should be
    (define (yoff) (nth 5 TEMPLATE-MIN-OFFSET-INC)) ; in original card coordinates [~rotation]


    ;; lx, ly are from ILXY, the 'sample-point' is at [xoff, yoff] or [TEMPLATE-RADI, TEMPLATE-RADI]
    (define (rotated-xy image layer lx ly)
      ;; return (x y) on template equiv to (lx ly) on un-rotated card.
      (let* ((srcw (car (gimp-image-width image)))
	     (srch (car (gimp-image-height image)))
	     (sp? (> srch srcw))	; src is portrait?
	     (tp? (> (cardw) (cardh)))	; template-slot is portrait?
	     (norot? (eq? sp? tp?))	; src is not rotated on template...?
	     (x (+ lx (if norot? (xoff) (- (cardh) (yoff)))))
	     (y (+ ly (if norot? (yoff) (xoff))))
	     )
	(list x y)))

    ;; return #t unless image[lx,ly] is open for a new card
    (define (is-card-at image layer lx ly)
      ;; catch & return #t if [lx,ly] is outside of image:
      (let* ((cx (+ lx TEMPLATE-ORIG-X))
	     (cy (+ ly TEMPLATE-ORIG-Y))
	     (xy (rotated-xy image layer cx cy))
	     (x (nth 0 xy))
	     (y (nth 1 xy)))
	(message-string "is-card-at:" lx ly "-->" cx cy "-->" x y)
	(catch #t (> (get-alpha image layer x y) 0))))

    ;; return (full? ilxy) if NOT full?, then ilxy is next slot on template-image
    ;; That is: get-next-slot
    (define (is-template-full? image layer x y)
      ;; Template is FULL if LAST (x,y) indicates the *NEXT* slot is off-template.
      ;; return (full? next-x next-y)
      ;;(message-string "is-template-full?:" image layer x y)
      (let* ((tempw (car (gimp-drawable-width layer))) ; size of this template image [right edge]
	     (temph (car (gimp-drawable-height layer)))	;
	     (xlim (- tempw (cardw) (xmin)))		; max x for placing a new card
	     (ylim (- temph (cardh))))
	;; assert: (x,y) is valid slot (FIRST or LAST-ILXY)
	(set! x (+ x (xinc)))		; check location of NEXT slot!
	;;(message-string "is-template-full?" x y)
	(when (> x xlim)
	  (set! x (xmin))
	  (set! y (+ y (yinc))))
	;;(message-string "is-template-full?" (> y ylim) x y)
	(list (> y ylim) (list image layer x y))
	))

    (define (next-slot-on-template . args)
      (let ((n (util-opt-arg args 1)))
	(while (> n 0)
	  (apply is-template-full? LAST-ILXY)
	  (set! n (- n 1)))))
    
    (define (next-ilxy image layer x y)
      ;; given LAST-ILXY or FIRST-ILXY == (-1 -1 xmin ymin)
      ;; return (image layer x y) of next template slot (maybe new image)
      (let* ((full-ilxy (is-template-full? image layer x y))
	     ;;(msg1 (message-string "next-ilxy: full-ilxy=" full-ilxy))
	     (full? (nth 0 full-ilxy))
	     (rv-ilxy (if full?
			  ;; open new (image layer xmin ymin) [or BACKFILL partial template]
			  (next-image-layer-xy image layer) 
			(nth 1 full-ilxy) ; ILXY of NEXT slot
			)))
	;;(message-string "next-ilxy: full?" full? "rv=" rv-ilxy)
	rv-ilxy
	))

    ;; return list (image layer x y) of corner of a new slot on template-image
    ;; assert (image layer) is valid.
    (define (next-ilxy-empty)
      (let ((ilxy LAST-ILXY) (count 0))
	;;(message-string "next-ilxy-empty entry:" ilxy)
	(while (apply is-card-at ilxy)		 ; (is-card-at ilxy)
	  (set! count (+ 1 count))
	  (if (> count 40) (throw "Looping:" count ilxy))
	  (set! ilxy (apply next-ilxy ilxy)))	 ; (next-ilxy ilxy)
	ilxy))

;;; return ILXY of empty slot on given image-template OR #f if template is full
    (define (next-empty-ilxy ilxy)
      ;; assert (image layer) is valid.
      (if (not (apply is-card-at ilxy)) ilxy
        (let ((full-ilxy (apply is-template-full? ilxy)))
          (if (car full-ilxy) #f (next-empty-ilxy (nth 1 full-ilxy))))))
  
;;; holding the FIRST-ILXY for each image (image layer (xmin) (ymin))
    (define open-ilxy '())     
    (define (get-open-ilxy) open-ilxy)
    (define (set-open-ilxy ilxy-list) (set! open-ilxy ilxy-list))

    (define use-backfill #t)
    (define (set-use-backfill val) (set! use-backfill val))
;;; return ILXY or #f
    (define (backfill-ilxy)
      (define (prune-open ilxy)
	(let ((open? (catch #f (next-empty-ilxy ilxy)))) ; #f if image is invalid/deleted
	  (if (not open?) (set-open-ilxy (cdr open-ilxy))) ; unshift when elt is not usable
	  open?))

      (let ((ilxy (list-search-positive open-ilxy prune-open))) ; next-empty-ilxy
	;;(message-string "backfill-ilxy" ilxy "open-ilxy" open-ilxy)
	ilxy))


    (define (image-base-layer image)
      ;; lowest layer of image
      (let* ((layers (gimp-image-get-layers image))
	     (nlayer (car layers))
	     (layerv (cadr layers)))
	(vector-ref layerv (- nlayer 1))))

    ;; image0 is the card-image, image is the template-image
    (define msg-next-image-layer #t)
    (define (next-image-layer-xy image0 layer0 . file)
      (and msg-next-image-layer (message-string "next-image-layer-xy:" image0 layer0 file "open-ilxy =" open-ilxy ))
      (or (and (not (pair? file)) use-backfill (backfill-ilxy) )
	  ;; get ilxy of a NEW template image
	  ;; Called with ( -1 -1 . file) by use-template [image-file]
	  ;; or *first* invocation: [TEMPLATE-FILE]
	  (let* ((filename (if (pair? file)
			       (car file) ; use given/new template name
			     (car (gimp-image-get-filename image)))) ; filename of current template
		 (image (next-template-image filename))		     ; -N+1.png
		 (layer (image-base-layer image))
		 (ilxy (list image layer (xmin) (ymin)))
		 )
	    (set-open-ilxy (cons ilxy (get-open-ilxy))) ; shift onto open-ilxy
	    (and msg-next-image-layer (message-string "next-image-layer-xy" filename "ilxy:" ilxy "LAST-ILXY" LAST-ILXY))
	    ilxy
	    )))
      
    (define (rename-to-dir dir oname pname . ext)
      ;; change directory and basename, retain "-N.png"
      ;; oname: basename-n.ext
      ;; pname: newname
      ;; return DIR/newname-n.ext
      (let* ((name-dot-type (strbreakup oname "."))			    ; (file-name-N png)
	     (name-num (strbreakup (car name-dot-type) "-"))		    ; (file name N)
	     (num (car (reverse name-num)))				    ; N
	     (type (if (pair? ext) (car ext) (car (reverse name-dot-type))))) ; "png" or [ext]
	(string-append dir pname "-" num "." type)))
    

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

    ;; either repeated (card-to-template...) OR explicit 'flush' by caller, with ((car alt) = #t)
    ;; return (car (is-template-full? LAST-ILXY)) [Mar 2022]
    (define (save-if-template-full . alt)
      ;; Save template associated with LAST-ILXY [exported API]
      ;; save IFF (XOR full? alt?)
      ;;(message-string "save-if-template-full: alt=" alt LAST-ILXY)
      (if (equal? TRUE (car (gimp-image-is-valid (car LAST-ILXY)))) ; ie: NOT immediately after (set-end-of-template)
	  (let ((full? (car (apply is-template-full? LAST-ILXY)))
		(alt? (if (pair? alt) (car alt) #f))) ; (alt (util-opt-arg alt #f))
	    ;;(message-string "save-if-template-full: " full? alt? alt)
	    (if (or (and full? (not alt?)) (and alt? (not full?)))
		(save-template LAST-ILXY) ; returns cname
	      )
	    )))

    ;; make a new, empty template image (&layer)
    (define (next-template-image filename) ; .../dir2/dir1/card-name-N.png
      ;; Extract -N.png and use -N+1.png
      ;; file name MUST end with "-N" (this image/file now "filled" with cards)
      ;; find/open version named "-0" [template-0.png, MyMiniCards18-0.png]
    
      ;; protection during debug
      (define (quit-on-limit n lim) (if (> n lim) (throw (stringifyf "quit-on-limit" n ">" lim))))

      (let* ((rslash (reverse (strbreakup filename DIR-SEPARATOR))) ; card-name-N.png dir1 dir2 ...
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
	(para-set-comment image oname)
	(para-set-display image display) ; record Display
	(which-empty image)		 ; reset empty calc (dubious? obsolete?)
	;;(message-string "next-template-image:" image layer display cname)
	image))


    (define export-list
      (exports next-ilxy next-ilxy-empty next-image-layer-xy save-if-template-full
	       next-template-image
	       backfill-ilxy use-backfill
	       image-base-layer
	       next-slot-on-template	; skip - [pro'ly make an open]

	       ;; to run sfct source-code in console:
	       next-empty-ilxy		; return empty slot (or #f if full)
	       get-open-ilxy		; all the open ilxy
	       set-open-ilxy
	       is-card-at xoff yoff xmin ymin
	       is-template-full? cardw cardh xinc yinc ;  xmin ymin
	       ))
    )
  )

(let ((exported (eval templ::export-list))) ; import into top-level environment
  (message-string "templ::exports =" exported))
		
(sf-reg "use-template" "y-Use Template" "restart with this template"
	"RGB* GRAY*"
	SF-IMAGE "Input Image (current image)" 0
	SF-DRAWABLE "Input Drawable (layer)" 0
	)

(sf-reg "script-fu-template-n" "z-N to Template" "set N times in template"
	"RGB* GRAY*"
	SF-IMAGE "Input Image (current image)" 0
	SF-DRAWABLE "Input Drawable (layer)" 0
	SF-VALUE "N reps" "0"
	)

(sf-reg "script-fu-template" "z-To Template" "set in current/next template"
	"RGB* GRAY*"
	SF-IMAGE "Input Image (current image)" 0
	SF-DRAWABLE "Input Drawable (layer)" 0
	)

(sf-reg "script-fu-template-debug" "z-To Template - Debug" "set in template"
	"RGB* GRAY*"
	SF-IMAGE "Input Image (current image)" 0
	SF-DRAWABLE "Input Drawable (layer)" 0
	)

(message-string "loaded script-fu-card-template")


;;; Local Variables:
;;; eval: (save-backup "~/Data/Programs/ng/citymap/src/app/cardinfo/script-fu-card-template.scm")
;;; End:
