(require "templates/script-fu-para-cards.scm" 'file-merge-and-save)
(message-string "loading templ.scm")

(define PPG-POKER-18-SPEC '((file "PPGPoker18-0.png") (cardw 1108) (cardh 808)
			    (xmin 120) (ymin 85) (xinc 1125) (yinc 825)
			    (ncol 3) (nrow 6) (bleed 25)))

(define MY-POKER-8-SPEC '((file "MyPoker8-0.png") (cardw 1108) (cardh 808)
			  (xmin 11) (ymin 85) (xinc 1125) (yinc 825)
			  (ncol 2) (nrow 4) (bleed 25)))

(define PPG-MINI-36-SPEC '((file "PPGMiniCard36-0.png") (cardw 800) (cardh 575)
			   (xmin 150) (ymin 100) (xinc 833) (yinc 578.25)
			   (over 1) (bleed 25) (xlim 3600) (ylim 5400))) ; 5400

(define MY-MINI-18-SPEC '((file "MyMiniCard18-0.png") (cardw 750) (cardh 525)
			  (xmin 84) (ymin 25) (xinc 752) (yinc 527)
			  (ncol 3) (ncol 6) (bleed 0)))

(define MY-MICRO-18-SPEC '((file "MyMiniCard18-0.png") (cardw 250) (cardh 175)
			  (xmin 84) (ymin 25) (xinc 752) (yinc 527)
			  (ncol 3) (ncol 6) (bleed 0))) ; dubious

(define MY-TOKEN-24-SPEC '((file "MyTokenSpec-0.png") (cardw 115) (cardh 115)
			   (xmin 50) (ymin 50) (xinc 117) (yinc 117)
			   (bleed 0) (radi 1) (xlim 750) (ylim 525) (save #f))) 

(define (Project proj . base)
  (make-environment
    (define BASE-DIR (if (pair? base) (car base) "/Users/jpeck/Google Drive/jpeckj/"))
    (define PROJECT-DIR proj)
    (define (dirpath . path) (apply string-append BASE-DIR proj path))
    (define IMAGE-DIR (dirpath "/images/"))
    (define CARD-DIR (dirpath "/cards/"))
    (define PUB-DIR  (dirpath "/publish/"))
    (define XCF-DIR  (dirpath "/xcf/"))
    (define TEMPLATE-DIR (dirpath "/templates/"))
  ))


;;;; GIMP helpers:
(define (get-alpha layer x y)
  (let* ((pxi (gimp-drawable-get-pixel layer x y))
	 (ndim (car pxi))
	 (pxary (cadr pxi)))
    (if (> ndim 3) (vector-ref pxary 3) -1)))

(def-para-get para-get-page-name "page-name")
(def-para-set para-set-page-name "page-name")

;; (define tt (Templ spec))
;;
;;;; EXPORTS: card-to-template, save-if-template-full

;;;; Entry-point:
(define (Templ tspec proj)
  ;; extract field names and orig/default values from assoc-list ['tspec']
  (macro (def-slot form)
    (let ((name (cadr form))
	  (rest (cddr form)))
      `(define ,name (util-assq (quote ,name) tspec ,@rest))))

  (make-environment
    ;; initialize 'fields'
    (define spec tspec)			; original spec [or should we (apply list spec) ?
    (define project proj)		; PROJECT
    (def-slot file #f)			; basename.png
    (define filepath (string-append project::TEMPLATE-DIR file))
    (def-slot open-ilxy-func #f)	; (open-ilxy-func templ file|#f)

    (def-slot over 0)			; OR: tweak published numbers
    (def-slot xmin  84)			; printer margin
    (def-slot ymin  25)			; printer margin
    (def-slot cardw 750)		; no bleed
    (def-slot cardh 525)		; no bleed
    (def-slot radi 37)			; ~1/8th inch; then add bleed
    (def-slot safe 25)
    (def-slot bleed 0)
    (set! xmin  (- xmin  over))
    (set! ymin  (- ymin  over))
    (set! cardw (+ cardw over over))
    (set! cardh (+ cardh over over))
    (set! safe  (+ safe  over))
    (set! bleed (+ bleed over))

    (def-slot ncol  3)
    (def-slot nrow  6)
    (def-slot xinc  cardw)		; 
    (def-slot yinc  cardh)		; 
    (def-slot xlim  (+ xmin (* ncol xinc)))
    (def-slot ylim  (+ ymin (* nrow yinc)))
    (def-slot origx 0)
    (def-slot origy 0)
    (def-slot tempw xlim)
    (def-slot temph ylim)
    (def-slot corner-radius (+ radi bleed))
    (def-slot xoff corner-radius)	; pixel to test for is-card-at
    (def-slot yoff corner-radius)	; pixel to test for is-card-at
    (def-slot save #t)			; change with (enable-save-template val)
    (def-slot save-template-enabled save)

    (define (info) (list file project::PROJECT-DIR LAST-ILXY open-ilxy))
    (define is-portrait? (< cardw cardh)) ; template-orientation
    (define LAST-ILXY)

    (define (set-orig-xy x y) (set! origx x) (set! origy y))

    ;; define 'methods'
    (define (set-ilxy ilxy . lxy)
      (set! LAST-ILXY (if (pair? ilxy) ilxy `(,ilxy ,@lxy))))

    (define (set-end-of-template)
      ;; mark LAST-ILXY as INVALID; with xmin, ymin for next template image
      ;; force open-ilxy-file
      (set-ilxy `(-1 -1 ,xmin ,ymin)))
    (set! LAST-ILXY (set-end-of-template))
    
    (define msg-ctt #f)
    ;;  get-empty-ilxy, card-to-this-ilxy & save-if-template-full
    (define (card-to-template image layer undo context)
      ;; image to next open slot, updating LAST-ILXY
      (and msg-ctt (message-string1 "ctt-0:" image layer undo context))
      (set-ilxy (get-empty-ilxy))		     ; possibly a new image-template?
      ;; assert LAST-ILXY is valid image & layer
      (and msg-ctt (message-string1 "ctt-2:" LAST-ILXY))
      (and undo (gimp-image-undo-group-start image)) ; mark for undo
      (card-to-this-ilxy image LAST-ILXY context)    ;
      (save-if-template-full)			     ; save now, if template IS full
      (and undo (gimp-image-undo-group-end image))
      (and msg-ctt (message-string "ctt-3: LAST-IXY" LAST-ILXY))
      )

    (define (get-empty-ilxy)
      (when (< (car LAST-ILXY) 0)
	(and msg-ctt (message-string "ctt-1: OPEN TEMPLATE:" file))
	(set-ilxy (open-ilxy-file file)))
      (next-ilxy-empty LAST-ILXY 40)
      )
      
    ;; return list (image layer x y) of corner of a new slot on template-image
    ;; assert (image layer) is valid.
    (define (next-ilxy-empty ilxy count)
      (if (< count 1) (throw "Looping:" ilxy))      
      (if (apply is-card-at ilxy) 
	  (next-ilxy-empty (apply next-ilxy ilxy) (- count 1))
	ilxy))

    ;; return #f if pixel at image[lx,ly] is transparent (or off template)
    (define (is-card-at image layer lx ly)
      ;; catch & return #t if [lx,ly] is outside of image:
      (let* ((cx (+ lx origx))
	     (cy (+ ly origy))
	     (xy (rotated-xy #f cx cy xoff yoff))
	     (rx (nth 0 xy))
	     (ry (nth 1 xy))
	     (alpha (catch 222 (get-alpha layer rx ry))))
	;;(message-string "is-card-at:" `(,image ,layer ,lx ,ly) "-->" cx cy "-->" rx ry "==>" alpha)
	(> alpha 0)))

    ;; lx, ly are from ILXY; dx, dy identify the 'sample point' of interest
    (define (rotated-xy portrait? lx ly dx dy) ; dxy: (xoff)(yoff)
      ;; return (x y) on template equiv to (lx ly) on un-rotated card.
      (let* ((sp? portrait?)		; src is portrait? mode
	     (tp? is-portrait?)		; template-slot is portrait?
	     (norot? (eq? sp? tp?))	; src is not rotated on template...?
	     (rx (+ lx (if norot? dx (- cardh dy))))
	     (ry (+ ly (if norot? dy dx))))
	(list rx ry)))

    (define (next-ilxy image layer x y)
      ;; return (image layer x y) of next template slot (maybe new image)
      (let* ((full-ilxy (is-template-full? image layer x y))
	     (msg1 (and msg-ctt (message-string "next-ilxy: full-ilxy=" full-ilxy)))
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
      (set! x (+ x xinc)) 		; advance to next slot
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

    (define msg-open-ilxy #f)

    ;; backfill existing open-ilxy template OR open a new template from filename
    ;; file: filename OR #t to increment name from current template-image
    (define (open-ilxy-file given-file)
      (and msg-open-ilxy (message-string "open-ilxy-file:" given-file file "open-ilxy =" open-ilxy ))
      (if open-ilxy-func
	  (open-ilxy-func templ given-file)
	(or (and (not given-file) use-backfill (backfill-ilxy)) ; try use backfill-ilxy
	    ;; get ilxy of a NEW template image
	    (let* ((image (open-template-file-image #f)) ; -N+1.png
		   (layer (image-base-layer image))
		   (ilxy (list image layer xmin ymin))
		   )
	      (if (not xlim) (set! xlim (car (gimp-image-width image))))
	      (if (not ylim) (set! ylim (car (gimp-image-height image))))
	      (set-open-ilxy (cons ilxy (get-open-ilxy))) ; shift onto open-ilxy
	      (and msg-open-ilxy (message-string "open-ilxy-file2" file "ilxy:" ilxy "LAST-ILXY" LAST-ILXY))
	      ilxy
	      ))
	))

    (define (image-base-layer image)
      ;; lowest layer of image
      (let* ((layers (gimp-image-get-layers image))
	     (nlayer (car layers))
	     (layerv (cadr layers)))
	(vector-ref layerv (- nlayer 1))))

    ;; holding the FIRST-ILXY for each image (image layer (xmin) (ymin))
    (define open-ilxy '())     
    (define (get-open-ilxy) open-ilxy)
    (define (set-open-ilxy ilxy-list) (set! open-ilxy ilxy-list))

    (define use-backfill #t)
    (define (set-use-backfill val) (set! use-backfill val))

    ;; return (next-empty-ilxy open-ilxy) or #f
    (define (backfill-ilxy)
      (define (prune-open ilxy)
	(let ((open? (catch #f (next-empty-ilxy ilxy))))   ; #f if image is invalid/deleted
	  (if (not open?) (set-open-ilxy (cdr open-ilxy))) ; unshift when elt is not usable
	  open?))

      (let ((ilxy (list-search-positive open-ilxy prune-open))) ; next-empty-ilxy
	(and msg-open-ilxy (message-string "backfill-ilxy" ilxy "open-ilxy" open-ilxy))
	ilxy))

    ;; return ILXY of empty slot on given image-template OR #f if template is full
    (define (next-empty-ilxy ilxy)
      ;; assert (image layer) is valid.
      (if (not (apply is-card-at ilxy)) ilxy
        (let ((full-ilxy (apply is-template-full? ilxy)))
          (if (car full-ilxy) #f (next-empty-ilxy (nth 1 full-ilxy))))))
  

    ;; GIMP stuff:

    (define (card-to-this-ilxy image ilxy context)
      ;; 'image' is a CARD
      ;; rotate/move image to ilxy & card-parasite-record-image
      (let* ((layer 0)			       ; src layer (not used)
	     (srcW (car (gimp-image-width image))) ; (card-width image)
	     (srcH (car (gimp-image-height image)))
	     (destImage (nth 0 ilxy))	; implied by destLayer
	     (destLayer (nth 1 ilxy))
	     (destX (+ (nth 2 ilxy) origx))
	     (destY (+ (nth 3 ilxy) origy)))
	;;(message-string "ctt-ilxy: ILXY=" ilxy "(image layer)" (list image layer))

	;; merge & clip
	(set! layer (car (gimp-image-merge-visible-layers image CLIP-TO-IMAGE)))
	(gimp-selection-all image)	; select ALL
	(gimp-edit-copy layer)		; copy selection to edit-buffer [layer not used]
	;; return should be TRUE [FALSE iff nothing to copy]
	(and context (gimp-context-push))
	(and context (gimp-context-set-defaults))

	(gimp-context-set-transform-resize TRANSFORM-RESIZE-ADJUST)
	(let* ((rotImage (car (gimp-edit-paste-as-new-image))) ; edit-paste, [rotate], edit-copy
	       (rotLayer (car (gimp-image-get-active-layer rotImage)))
	       (floatLayer 0))
	  (maybeRotate rotLayer (< srcW srcH) is-portrait?)
	  (gimp-edit-copy rotLayer)			       ; rotated image to edit-buffer
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
	))

    (define (maybeRotate layer os od)
      ;; if orientations differ, then rotate 90-degrees:
      ;; TODO: scale to match (tempw, temph)??
      (if (or (and os (not od)) (and od (not os)))
	  (gimp-item-transform-rotate-simple layer ROTATE-90 FALSE 0 0)))
    
    ;; FILE stuff:

    ;; .../project/publish/Template-XX-0.png [N > 0] or #f
    (define last-template-filename #f)
    ;; make a new, empty template image (&layer)
    ;; when invoked with a filename, it is likely "filename-0.png" [the zeroth/original template file]
    ;; if filename == #f, then proceed to *next* incremental filename [max++]
    (define (open-template-file-image given-file) ; .../dir2/dir1/Template-XX-N.png [N == 0] or #f
      ;; Extract -N.png and use -N+1.png
      ;; file name MUST end with "-N" (this image/file now "filled" with cards)
      ;; find/open version named "-0" [template-0.png, MyMiniCards18-0.png]
    
      ;; protection during debug
      (define (quit-on-limit n lim) (if (> n lim) (throw (stringifyf "quit-on-limit" n ">" lim))))

      (let* ((filename (or given-file last-template-filename filepath))
	     (rslash (reverse (strbreakup filename DIR-SEPARATOR))) ; Template-XX-N.png dir1 dir2 ...
	     (name-dot-type (strbreakup (car rslash) "."))	    ; (Template-XX-N png)
	     (dot-type (string-append "." (nth 1 name-dot-type)))   ; .png
	     (name-num (strbreakup (car name-dot-type) "-"))	    ; (Template XX N)
	     (num-names (reverse name-num))			    ; (N XX Template)
	     (basename (unbreakupstr (reverse (cdr num-names)) "-")) ; Template-XX
	     (new-num (+ 1 (string->number (car num-names))))	     ; N+1
	     (num-str (number->string new-num))			     ;"N+1"
	     ;;(xxx (quit-on-limit new-num 10))
	     (nname (string-append basename "-" num-str dot-type)) ; Template-XX-N+1.png
	     (cname (string-append project::PUB-DIR "/" nname ))   ; PUB-DIR/Template-XX-N+1.png
	   
	     ;; open templ::filepath and set next filename
	     (image (car (gimp-file-load RUN-NONINTERACTIVE filepath nname))) ; blank template
	     (rename (gimp-image-set-filename image cname))		      ; save to PUB-DIR
	     (display (car (gimp-display-new image))) ; which display/tab shows image
	     )
	(set! last-template-filename cname) ; save new filename: PUB-DIR/Template-XX-N+1.png
	(para-set-page-name image (or (para-get-global-pubname) basename)) ; last set page name
	(para-set-comment image nname)	    ; the short name
	(para-set-display image display)    ; record Display
	;;(message-string "open-template-file-image:" image (image-base-layer image) display cname)
	image))

    ;; either repeated (card-to-template...) OR explicit 'flush' by caller, with ((car alt) = #t)
    ;; return (car (is-template-full? LAST-ILXY)) [Mar 2022]
    (define (save-if-template-full . arg)
      ;; Save template associated with LAST-ILXY [exported API]
      ;; save IFF (XOR full? alt?)
      (and msg-save-if-full (message-string "save-if-template-full0: arg=" arg LAST-ILXY))
      (if (equal? TRUE (car (gimp-image-is-valid (car LAST-ILXY))))
	  ;; ie: NOT immediately after (set-end-of-template)
	  (let ((full? (car (apply is-template-full? LAST-ILXY)))
		(alt? (util-opt-arg arg #f))) ; (if (pair? arg) (car arg) #f))) ; 
	    (and msg-save-if-full (message-string "save-if-template-full1: " full? alt? arg))
	    (if (or (and full? (not alt?)) (and alt? (not full?)))
		(save-template LAST-ILXY) ; returns cname
	      )
	    )))

    (define (save-template ilxy)
      ;; save given image (now ~full of cards) to filename-N.png:
      ;; return complete name of the saved file
      ;;(message-string "save-template" (template-get-spec))
      (when save-template-enabled
	(let* ((image (nth 0 ilxy))
	       (layer (nth 1 ilxy))
	       (cname (car (gimp-image-get-filename image))) ; PUB-DIR/template-name-N.png
	       (nname (util-file-basename cname)) ; template-name-N.png [para-get-comment]
	       (pname (para-get-page-name image)) ; "Name" (0 page "Name") or "Template-XX"
	       (sname (if pname
			  (rename-to-dir nname project::PUB-DIR pname) ; PUB-DIR/Pname-N.png
			cname))					       ; PUB-DIR/Tname-N.png
	       (xname (rename-to-dir nname project::XCF-DIR pname "xcf"))
	       (mode RUN-NONINTERACTIVE))
	  (message-string "save-template publish:" sname nname)
	  (para-set-comment image pname)
	  (gimp-file-save mode image layer sname nname) ; save .PNG/.JPG in PUB-DIR
	  (gimp-xcf-save  mode image layer xname nname) ; save .XCF in XCF-DIR
	  cname
	  )))

    (define (rename-to-dir nname dir pname . ext)
      ;; change directory, basename, maybe ext: retain "-N" or "-N.png"
      ;; nname: basename-N.png
      ;; pname: new Pubname
      ;; return DIR/Pubname-n.ext
      (let* ((name-dot-type (strbreakup nname "."))	     ; (file-name-N png)
	     (name-num (strbreakup (car name-dot-type) "-")) ; (file name N)
	     (num (car (reverse name-num)))		     ; N
	     (type (if (pair? ext) (car ext) (car (reverse name-dot-type))))) ; "png" or [ext]
	(string-append dir pname "-" num "." type)))

    (define force-display-template #t)
    (define (set-force-display val) (set! force-display-template val))

    (define msg-save-if-full #t)
    (define (set-msg-save-if-full val) (set! msg-save-if-full val))

    ;; set to #f to prevent writing of any template
    (define (enable-save-template value) (set! save-template-enabled))

    ))

(message-string "templ loaded")
