;; load utils.scm if not already loaded:
(require "lib/utils" util-opt-arg)			   ; so macros are defined
(require "templates/script-fu-card-template" ENTRY-POINTS) ; load in the proper order...

;; Angular-11 (> 8) ng serve does not build assets across symlink. (Jan 2021)
;; NOW: "citymap" project directs finished cards to .../ng/citymap/src/assets/main/images/cards/  (citymap/cards -> .../ng/.../cards)

(macro (ifgimp let-rest)
  (let* ((letform (cadr let-rest))
	 (rest (cddr let-rest)))
    `(let* ((image-layer (if DO-GIMP ,letform `(#f 0)))) ,@rest image-layer)))

;; card-make-deck sets DO-GIMP & CARD-DISPLAY? [also C-M-a, C-M-p]
;; DO-GIMP vs JUST-DO-INFO    [DO-TEMPLATE requires DO-GIMP, ONLINE could be either]
;; DISPLAY? vs JUST-WRITE-PNG [DISPLAY? == #f is faster, esp for JUST-DO-INFO]
;; DO-TEMPLATE vs CARD-DISPLAY?
;; DO-INFO

;; DO-GIMP & DO-TEMPLATE & !DO-DISPLAY (make all the images, put on template, no DISPLAY of images)
;; DO-GIMP & !DO-TEMPLATE & DO-INFO (make card image & write info-file)
;; !DO-GIMP & !DO-TEMPLATE & DO-INFO (just write info-file)

;; CARD-SAVE-PNG? [so can DO-GIMP, but not save file? for testing?]

(define DO-GIMP #t) ; #t: do GIMP image generation, #f: Just write cardinfo files (set by sf-make-deck/info)
(define CARD-SCALE .5)		     ; scale when write PNG [template always uses full-scale image]

;;; Card-images are always (750x525) 

(template-use "citymap" MY-MINI-18-SPEC)  ; use card-size for MY-MINI-18-SPEC (trimmed: no bleed)
(define DO-INFO #t)		     ; #t: write info to typescript class file
(define DO-TEMPLATE #f)		     ; #t: card-put-image on template-image VS card-write-info
(define CARD-DISPLAY? #t)	     ; #t: card-put-image also force DISPLAY for each card-image
(define CARD-SAVE-PNG #t)	     ; #t: if DO-GIMP write .PNG; #f: TEST/TEMPLATE: no CARD.PNG
(define CARD-SIZE TEMPLATE-CARD-SIZE)	; nominal: (750 525) no bleed

(define (no-bleed-template bleed template)
  (let* ((file (car template))
	 (size (cadr template))
	 (offset (cddr template))
	 (w (nth 0 size))
	 (h (nth 1 size))
	 (x (nth 0 offset))
	 (y (nth 1 offset))
	 (sizeb (list (- w bleed bleed) (- h bleed bleed)))
	 (offsb `(,(+ bleed x) ,(+ bleed y) ,@(cddr offset)))
	 )
    `(,file ,sizeb ,@offsb)))

(define PPG-MINI-36-BLEED (no-bleed-template 25 PPG-MINI-36-SPEC))
(template-use "Estates" PPG-MINI-36-BLEED) ; (750 525) with room to bleed

;;(template-use "Estates" PPG-MINI-36-SPEC) ; (800 575) includes bleed space
(define DO-INFO #f)		     ; #t: write info to typescript class file
(define DO-TEMPLATE #t)		     ; #t: card-put-image on template-image VS card-write-info
(define CARD-DISPLAY? #f)	     ; #t: card-put-image also force DISPLAY for each card-image
(define CARD-SAVE-PNG #f)	     ; #t: if DO-GIMP write .PNG; #f: TEST/TEMPLATE: no CARD.PNG
(define CARD-SIZE TEMPLATE-CARD-SIZE)

(define CARD-LOOP #t)		   ; #f to stop generation loops [tends to kill script-fu/GIMP]
(define CIRCLE-IMAGE-SIZE 250)     ; [for Debt & House Tokens]


;; GIMP uses these (R G B) for color:
(define BLACK  '(0    0   0))
(define GREY   '(128 128 128))
(define WHITE  '(255 255 255))
(define BROWN  '(185 83 0))
(define DEBT   '(224 92  0)) ; #e05c00
(define GOLD   '(235 188 0))
(define RED    '(239 32 60))
(define ORANGE '(255 128 0))
(define YELLOW '(255 255 0))
(define GREEN  '( 21 180 0))
(define BLUE1  '( 36 36 255))		; dark
(define BLUE   '(162 162 255))		; lighter, purpler
(define PURPLE '(232 115 255))

(define TRANSIT-COLOR  '(180 180 180))
(define COM-TRANSIT-COLOR  '(180 120  80)) ; someday do BROWN/GREY stripes!

(define CENTER TEXT-JUSTIFY-CENTER)
(define LEFTJ  TEXT-JUSTIFY-LEFT)
(define RIGHTJ TEXT-JUSTIFY-RIGHT)
(define TIMESNR "Times New Roman,")
(define ROUNDED ".SF Compact Rounded Medium")
(define TEXTFONT ROUNDED)		; use TIMENR or ROUNDED for all non-numeric Text

(define CARD-RADIUS 37)			; in GUI: 15% of 525/2 ~34 px; 37.5 = 1/8"
(define CARD-EDGE 25)			; safe-edge
(define CARD-TOP-BAND 115)		; Band at top for title [25 25 40 25] [safe,asc,font,desc]
(define CARD-COIN-BAND 98)		; Band near top for: Step, Stop, Rent
(define CARD-BOTTOM-BAND 120)		; Band at bottom for type, cost, VP


(define CARD-TITLE-FONT TEXTFONT)
(define CARD-TITLE-SIZE 60)
(define CARD-TEXT-FONT TEXTFONT)
(define CARD-TEXT-SIZE 50)
(define CARD-TYPE-FONT TEXTFONT)
(define CARD-TYPE-SIZE 40)
;; using 82% of diameter as size (~72 for main coins)
(define CARD-COIN-FONT ".SF Compact Rounded Medium")
(define CARD-COIN-SIZE 90)		; for Cost, Step, Stop, Rent
(define CARD-VP-FONT ".SF Compact Rounded Bold")
(define CARD-VP-SIZE 70)
(define DIR-TYPE-FONT ".SF Compact Rounded Bold")

(define context #t)

(define (card-width image)  (car (gimp-image-width image)))
(define (card-height image) (car (gimp-image-height image)))
(define (card-scale dim) (floor (* dim CARD-SCALE)))

(gimp-message "card define CARD functions")

(define (card-select-rr image . radius)
  (let ((w (card-width image))
	(h (card-height image))
	(r (if (pair? radius) (car radius) CARD-RADIUS)))
    (gimp-image-select-round-rectangle image CHANNEL-OP-REPLACE 0 0 w h r r)))

(define (card-select-edge-rect image x y w h)
  ;;(message-string1 "card-select-edge-rect" image x y w h)
  (card-select-rr image)
  (gimp-image-select-rectangle image CHANNEL-OP-INTERSECT x y w h))
  
(define card-force-display #f)
(define (card-make-base-image w h . args)
  ;; new card, WHITE Background layer
  (let* ((radius (util-opt-arg args CARD-RADIUS))
	 (image (car (gimp-image-new w h RGB)))
	 (layer (car (gimp-layer-new image w h RGBA-IMAGE "base-layer" 100 LAYER-MODE-NORMAL))))
    (gimp-drawable-fill layer FILL-WHITE)
    (gimp-image-insert-layer image layer 0 0) ; parent 0 position 0
    (card-select-rr image radius)
    (gimp-selection-invert image)
    (gimp-drawable-edit-fill layer FILL-TRANSPARENT)
    ;; CAN FORCE DISPLAY
    (when card-force-display
	  (display (card-put-image image 0 #t #f))
	  (let ((display (para-get-display image)))
	    (message-string "card-make-base-image:" `(image ,image layer ,layer display ,display))))
    ;; WHILE TESTING
    (list image layer)
    ))

(define (card-base-layer image)
  (let* ((layers (gimp-image-get-layers image))
	 (nlayer (car layers))
	 (layerv (cadr layers)))
    (vector-ref layerv (- nlayer 1))))

(define (eval-sym val)
  ;; unquotify symbol if necessary
  (if (symbol? val) (eval val) val))

(macro (set!-eval-sym form)
  (let ((val (cadr form)))
    `(if (symbol? ,val) (set! ,val (eval ,val)))))

(define (card-make-image-background image ty by color)
  ;; fill band of color at top and bottom of image background layer
  (gimp-context-set-foreground (eval-sym color))

  (let* ((xw (card-width image))
	 (yh (card-height image))
	 (layer (card-base-layer image)))
    (card-select-edge-rect image 0 0 xw ty)
    (gimp-drawable-edit-fill layer FILL-FOREGROUND)
    (card-select-edge-rect image 0 (- yh by) xw by)
    (gimp-drawable-edit-fill layer FILL-FOREGROUND)
    (gimp-selection-none image)
    (gimp-context-set-foreground BLACK)
    layer))

(define (template-width portrait?)
  (apply (if portrait? min max) CARD-SIZE))

(define (template-height portrait?)
  (apply (if portrait? max min) CARD-SIZE))
  
(define (card-make-base portrait? color ty by)
  ;; portrait #t or #f (for landscape)
  ;; color of CARD-TOP-BAND and CARD-BOTTOM-BAND
  ;;(message-string1 "card-make-base: portrait=" portrait color)
  (let* ((w (template-width portrait?))
	 (h (template-height portrait?))
	 (image-layer (card-make-base-image w h))
	 (image (car image-layer))
	 (layer (cadr image-layer)))
    ;;(message-string1 "card-make-base: portrait=" portrait w h)
    (card-make-image-background image ty by color)
    image-layer
    ))

(define msg-make-coin #f)
(define (card-make-coin image value cx cy size . args)
  ;; put a "coin" icon with value on card [radius of size pixels]
  ;; cx,cy can use 'center or 'left, etc.
  ;; .oval for Big Center Coin
  ;; layer, select-circle, fill-gold, set-value, merge-down, crop-to-content, set-offsets
  (and msg-make-coin (message-string1 "card-make-coin" image value cx cy size args))
  (let* ((oval (util-assq 'oval args 1))
	 (r180? (util-assq 'r180 args)) ; #t or #f
	 (font (util-assq 'font args CARD-COIN-FONT))
	 (gold (util-assq 'gold args GOLD))
	 (alpha (util-assq 'alpha args 100))			  ; 0--100% opaque
	 (valu (if (number? value) (number->string value) value)) ; "*"
	 (name (string-append "Coin:" valu))
	 (layer (car (gimp-layer-new image size size RGBA-IMAGE name 100 LAYER-MODE-NORMAL))))
    (and msg-make-coin (message-string "card-make-coin:" image layer valu "gold:" gold "oval:" oval))
    (gimp-image-insert-layer image layer 0 0) ; parent 0 position 0
    (gimp-layer-set-offsets layer 0 0)
    (if (string? oval) (set! oval (string->number oval)))
    ;; x0 y0 w h
    (gimp-image-select-ellipse image CHANNEL-OP-REPLACE (/ (* size (- 1 oval)) 2) 0 (* size oval) size)
    (gimp-context-set-foreground gold)	;
    (gimp-drawable-edit-fill layer FILL-FOREGROUND)
    (gimp-layer-set-opacity layer alpha) ; so set-step can make invisible gold
    (gimp-selection-none image)
    (gimp-context-set-foreground BLACK)	;
    ;; make a small layer in upper left corner:
    (let* ((c (/ size 2))
	   (x (- (normalize-x image cx) c))
	   (y (- (normalize-y image cy) c))
	   (font-size (floor (* .82 size))) ; expect Font=90 on Size=110
	   (ty (if (equal? valu "*") (* (set! font-size size) .05) 0))
	   (tl (card-make-text-layer image valu c ty CENTER font-size font BLACK nil))
	   (layer2 (car (gimp-image-merge-down image tl CLIP-TO-BOTTOM-LAYER))))
      ;; if r180?: rotate layer2 by 180; flip x=-x, y=-y and normalize; 
      ;; x = (normalize-x image (- x)) y = (normalize-y (- y))
      (gimp-layer-set-offsets layer2 x y)
      (if r180?
	  (let ((cx (/ (card-width image) 2))
		(cy (/ (card-height image) 2)))
	    (gimp-item-transform-rotate-simple layer2 ROTATE-180 FALSE cx cy)))
      layer2)))

(define (card-justified-offset nx width justify)
  (case justify
    ((LEFTJ 0) nx)
    ((RIGHTJ 1) (- nx width))
    ((CENTER 2) (- nx (/ width 2 )))
    (else nx)))

(define (card-text-layer-width text size fontname)
  ;; 'nx, ny, justify' not used..
  (nth 0 (gimp-text-get-extents-fontname text size PIXELS fontname)))

(define (card-text-layer-height text size fontname)
  (nth 1 (gimp-text-get-extents-fontname text size PIXELS fontname)))

(define (card-text-layer-offsets text nx ny justify size fontname)
  ;; find lx to achieve CENTER or RIGHTJ
  (let* ((extents (gimp-text-get-extents-fontname text size PIXELS fontname))
	 (width (nth 0 extents))
	 (height (nth 1 extents))
	 (lx (card-justified-offset nx width justify)))
    (list lx width height)))

(define (card-shrink-font-for-width xwide text size fontn)
  ;;(message-string1 "card-shrink-font-for-width" xwide text size fontn)
  ;; for any region width
  (let* ((width (card-text-layer-width text size fontn)))
    (if (> width xwide) (floor (/ (* size xwide) width)) size)))

(define (card-font-width-for-card image text size fontname)
  ;; using default card-width
  (let* ((xwide (- (card-width image) CARD-EDGE CARD-EDGE)))
    (card-shrink-font-for-width xwide text size fontname)))


(define (card-tweak-text-layer tl tweaks)
  ;; apply tweaks to text-layer: tweaks (list (key value) ...)
  (carloop tweaks (lambda (tweak)
	 (let* ((key (car tweak))
		(value (eval-sym (cadr tweak))))
	   (case key
	     ((kerning) (gimp-text-layer-set-kerning tl value))
	     ((letter-spacing) (gimp-text-layer-set-letter-spacing tl value))
	     ((line-spacing) (gimp-text-layer-set-line-spacing tl value))
	     ((color) (gimp-text-layer-set-color tl value))
	     ((indent) (gimp-text-layer-set-indent tl value))
	     ((font) )			;nothing to do at this point
	     ((bold) )			;nothing to do at this point
	     ((size) )			;nothing to do at this point
	     ((lineno))			;nothing to do
	     (else (message-string1 "Unknown Tweak:" key value))
	     ))))
  tl)

(define (card-make-text-coin image text x y justify size fontname color tweaks)
  ;; textlist (strbreakup text "$") ("pay "$"2 + "$"1 per tile")
  ;; textlist (strbreakup text "$") ("pay "$"2")
  ;; (> (length textlist) 1)
  ;; paint each piece of textlist, at endpoint draw coin
  ;; some $<value> have space after, some do not [being end of string]
  ;; disallow "pay $2." ; must have " " after value (or end of string)
  ;; the LAST elt of text list does not end with $<value>.

  (define (text-coin text value x y size fontname)
    ;; return new X (for next text)
    ;;(message-string1 "text-coin" (stringify text) value x y size)
    (let* ((extents (gimp-text-get-extents-fontname text size PIXELS fontname))
	   (width (nth 0 extents))
	   (height (nth 1 extents))
	   (size2 (/ size 2))
	   (space -3)
	   (cx (+ x width space size2 ))
	   (cy (+ y (/ height 2)))
	   (clayer (card-make-coin image value cx cy size))
	   (endx (+ x width space size space))
	   )
      endx
      ))

  ;; "pay $1 + $2 per tile or $3" ("pay " "1 + " " per tile or " "3")
  ;; "pay $1 to move again or $4 more" ("pay " "1 to move again or " "4 more")
  (define (write-frags-with-coin-values text0 x0 y0 size rest) ; AMBIENT (image ... justify fontname color size tweaks)
    
    (if (pair? rest)			     ; text0 ended with $, rest starts with value
	;; write text0 ending with $VALUE in text1
	(let* ((tweaks ())		     ; suppress interference?
	       (text1 (nth 0 rest))	     ; "1 to move again" or "3"
	       (nstr (strbreakup text1 " ")) ; ("1" "to" "move" "again") OR ("4" "more") OR ("3")
	       (valu (nth 0 nstr))	     ; "1" or "*"...
	       (rstr (if (> (length nstr) 1)
			 (string-append " " (unbreakupstr (cdr nstr) " ")) "")) ; " to move" OR "" OR "more"
	       (endx (text-coin text0 valu x0 y0 size fontname)))
	  ;;(message-string1 "w-f-w-coin-values" (stringify text0) x0 y0 size endx rest)
	  (card-text-layer-simple image text0 x0 y0 LEFTJ size fontname color tweaks)
	  ;; recurse to do next frag:
	  (write-frags-with-coin-values rstr endx y0 size (cdr rest)) ; (" + " "2 per tile or ") OR ()
	  )
	;; write string NOT ending with $VALUE
	(card-text-layer-simple image text0 x0 y0 LEFTJ size fontname color tweaks))
    )

  (define (write-line-maybe-coin-values line x y0) ; AMBIENT (image ... justify fontname color size tweaks)
    (let* ((textlist (strbreakup line "$"))
	   (size2 (card-font-width-for-card image line size fontname)) ; shrink-to-fit
	   ;; offset to [LEFTJ] justified text
	   (offsets (card-text-layer-offsets line x y0 justify size2 fontname))
	   (x0 (nth 0 offsets))
	   (text0 (nth 0 textlist)))
      ;; textlist: ("frag0" . ("frag1"))
      ;;(message-string1 "w-l-w-coin-values" (stringify text0) x0 y0 (stringify (cdr textlist)))
      (write-frags-with-coin-values text0 x0 y0 size2 (cdr textlist))
      ;;(message-string1 "w-l-w-coin-values done" y0 (last textlist))
      y0))

  ;; -----------------------------
  ;; (card-make-text-coin image text x y justify size fontname color tweaks)

  ;;(message-string1 "card-make-text-coin" (stringify text) x y tweaks)
  (let* (;; check tweaks for 'line-spacing & 'indent
	 (lns (or (util-assq 'line-spacing tweaks) 0))
	 (ind (or (util-assq 'indent tweaks) 0))
	 (siz (or (util-assq 'size tweaks) size))
	 (tmp (set! size siz))		; for ambient bind all above
	 (lny (card-text-layer-height "X" size fontname))
	 (x0 (+ x ind))			; not both of 'indent and 'justify
	 (y0 y)
	 (lines (strbreakup text "\n"))
	 )
    ;;(message-string1 "card-make-text-coin2" x0 y0 (stringify lines))
    (carloop lines			; carloop1 to debug
     (lambda(line)
       ;;(message-string1 "card-make-text-coin3" (stringify line) "justify=" justify)
       ;; possibly indented first line
       (set! y0 (+ (write-line-maybe-coin-values line x0 y0) lns lny))
       ;;(message-string1 "card-make-text-coin done with line:" (stringify line))
       (set! x0 x))			; unindent thereafter
     ))
  )

(define (card-get-effective-text text . tweaks)
  ;; separate text from tweaks
  (when (pair? text)
	(set! tweaks (cdr text))
	(set! text (car text)))
  (list text tweaks))

(define (card-text-layer-simple image text nx ny justify size fontname color tweaks)
  ;; normalize-x, normalize-y, justify=CENTER, fonname=TEXTFONT=ROUNDED, color=BLACK, tweaks=()
  ;;(message-string1 "card-text-layer-simple" image (stringify text) nx ny justify size fontname color tweaks)
  ;; shrink text to fit at most card-width [unless size in tweaks]
  (let* ((xwide (- (card-width image) CARD-EDGE CARD-EDGE))
	 (size2 (util-assq 'size tweaks (card-font-width-for-card image text size fontname)))
	 (tl (car (gimp-text-layer-new image text fontname size2 PIXELS))))
    ;;(message-string1 "card-text-layer-simple2" (stringify text) nx ny xwide size2)
    (gimp-image-insert-layer image tl 0 0) ; 0 parent, 0 depth
    (gimp-text-layer-set-color tl color)
    (card-tweak-text-layer tl tweaks)
    (gimp-text-layer-set-justification tl justify)
    (let* ((width (car (gimp-drawable-width tl)))
	   (lx (card-justified-offset nx width justify)))
      (gimp-layer-set-offsets tl lx ny))
    tl)
  )


(define (card-make-text-layer-and-tweak image text x y justify size fontname color . tweaks)
  ;; when called with spec (.... (text "content" (tweak 1) (tweak 2)))
  ;; to put arbitrary text/tweaks [vs card-set-text using standard location/size/font]
  (card-make-text-layer image text x y justify size fontname color tweaks))

(define (card-make-text-layer image text x y justify size fontname color tweaks)
  ;;(message-string1 "card-make-text-layer" image (stringify text) x y justify size fontname color tweaks)
  ;; color FOREGROUND ?
  ;; size in PIXELS
  ;; if center? align center of text with X
  (cond
   ((null? text))			;do nothing
   ((or (string? text) (pair? text))
    (set!-eval-sym fontname)    	; unquotify as necessary
    (set!-eval-sym justify)
    (set!-eval-sym color)
    ;; text may be list ("content" . tweaks)
    (when (pair? text)
	  (set! tweaks (cdr text))
	  (set! text (car text)))
    ;; tweak ('font "Override Font Name")
    (when (util-assq 'font tweaks)
	  (set! fontname (util-assq 'font tweaks)))
    ;; tweak ('color "Override text color")
    (when (util-assq 'color tweaks)
	  (set! color (util-assq 'color tweaks)))
    ;; tweak ('bold #t)
    (when (util-assq 'bold tweaks)
	  ;; (message-string1 "Try boldify text:" fontname tweaks)
	  ;; boldify the font (if not already Bold)
	  (if (or (string-ends-with fontname " " "Light")
		  (string-ends-with fontname " " "Medium"))
	      ;; Have remove (Medium, etc)
	      (set! fontname (string-trim-end fontname " ")))
	  (if (not (string-ends-with fontname " " "Bold"))
	      (if (string-ends-with fontname "," "")
		  ;; "TNR ," is different:
		  (set! fontname (string-append fontname "Bold"))
		  (set! fontname (string-append fontname " Bold")))))

    (set! x (normalize-x image x))	; 'center -> (card-width image)/2
    (set! y (normalize-y image y))

    (if (string-index text #\$)
	(card-make-text-coin    image text x y justify size fontname color tweaks)
	(card-text-layer-simple image text x y justify size fontname color tweaks)))
   (else (message-string1 "card-make-text-layer: NOT A STRING" (stringify text)))
   )
  )

(define (card-make-dot image color size cx cy)
  ;;(message-string1 "card-make-dot" image color size cx cy)
  (let* ((c (/ size 2))
	 (layer (car (gimp-layer-new image size size RGBA-IMAGE "DOT" 100 LAYER-MODE-NORMAL))))
    ;; make a small layer in upper left corner:
    (gimp-image-insert-layer image layer 0 0) ; parent 0 position 0
    (gimp-layer-set-offsets layer 0 0)	      ; redundant...
    (gimp-image-select-ellipse image CHANNEL-OP-REPLACE 0 0 size size)
    (gimp-context-set-foreground color)
    (gimp-drawable-edit-fill layer FILL-FOREGROUND)
    (gimp-selection-none image)
    (gimp-context-set-foreground BLACK)	;
    ;; move to target:
    (let* ((x (- (normalize-x image cx) c))
	   (y (- (normalize-y image cy) c)))
      (gimp-layer-set-offsets layer x y)
      layer)))

;;; TODO: make these relative to current selection region
(define (normalize-x image x)
  (let ((width (card-width image))) ;; 375 (half size)
    (case x
      ((left) CARD-EDGE)
      ((right) (- width CARD-EDGE))
      ((center) (/ width 2))
      (else (if (>= x 0) x (+ x width))))))

(define (normalize-y image y)
  (let* ((height (card-height image))
	 ;; see also: card-white-region
	 ;;(bounds (gimp-selection-bounds image))
	 ;;(is-sel (nth 0 bounds))
	 ;;(height2 (if is-sel (- (nth 4 bounds)(nth 2 bounds)) (card-height image)))
	 )
    (case y
      ((top) CARD-EDGE)
      ((bottom) (- height CARD-EDGE))
      ((center) (/ height 2))
      (else (if (>= y 0) y (+ y height))))))

(define (card-dashify-name-ext name ext)
  (string-append (unbreakupstr (strbreakup name " ") "-") ext))

(define (card-filename-from-title title . args)
  (let ((ext (util-opt-arg args ".png")))
    (string-append CARD-DIR (card-dashify-name-ext title ext))))

(define (card-set-title image title . tweaks)
  ;; set Title and Filename
  (message-string1 "card-set-title" title tweaks)
  (let* ((filen (util-assq 'filen tweaks title))
	 (color (util-assq 'color tweaks BLACK))
	 (size CARD-TITLE-SIZE) (fontn CARD-TITLE-FONT))
    (card-make-text-layer image title 'center 'top CENTER size fontn color nil)
    (gimp-image-set-filename image (card-filename-from-title filen)))
  )

(define (card-price-bar-height image)
  ;; if there is a price bar, return its height
  (let* ((layer (card-base-layer image))
	 (color (car (gimp-image-pick-color image layer 30 (+ 30 CARD-TOP-BAND) FALSE TRUE 4))))
    (if (> (color-dist color WHITE) 32) CARD-COIN-BAND 0)))

(define (card-set-text image text . args)
  ;; text may be list ("content" . tweaks)
  ;;(message-string1 "card-set-text" image (stringify text))
  (let* ((lead (util-assq 'lead args (* 2 6))) ; 2x line "spacing" of 50pixel font
	 (size CARD-TEXT-SIZE) (font CARD-TEXT-FONT)
	 (top (+ lead CARD-TOP-BAND (card-price-bar-height image))))
    ;; card-set-text is not given any "tweak" (unless embedded in text)
    (card-make-text-layer image text 'center top CENTER size font BLACK nil)))
  
(define (card-set-text-low image text . tweaks)
  ;; horizontal bar and text at bottom of card (above BOTTOM-BAND)
  ;;(message-string1 "card-set-text-low" image text tweaks)
  (when (pair? text)
	(set! tweaks (cdr text))
	(set! text (car text)))
  (if (string? text)
      (let* ((size0 CARD-TEXT-SIZE) (font CARD-TEXT-FONT)
	     ;; shrink font before text-layer-height
	     (size (card-font-width-for-card image text size0 font))
	     (lead (* 2 6))		; 3x line "spacing"
	     (height (card-text-layer-height text size font))
	     (liney (- 0 (+ CARD-BOTTOM-BAND height lead lead)))
	     (texty (- 0 (+ CARD-BOTTOM-BAND height lead)))
	     (margin 40) (thick 5)
	     )
	(card-set-line image liney BLACK margin thick)
	(card-make-text-layer image text 'center texty CENTER size font BLACK tweaks)
	)
      ))

(define (card-set-type image text . tweaks)
  ;;(message-string1 "card-set-type" image text tweaks)
  ;; args=[lineno] put subtype after nth line.
  ;; line=0 is always CARD-TYPE-SIZE (no shrink)
  ;; line=1 starts below that, and may be shrunk.
  (define (lineoff lineno)
    (let* ((fontn CARD-TYPE-FONT) (size CARD-TYPE-SIZE)
	   (lineh (card-text-layer-height "M" size fontn)))
      ;;(message-string1 "card-set-type:" lineno size "Height=" lineh)
      (* lineno lineh)))
  ;; --------------------------
  
  (let* ((color (util-assq 'color tweaks BLACK))
	 (lineno (util-assq 'lineno tweaks)) ; subtype: (lineno 1)
	 (fontn CARD-TYPE-FONT)
	 (xwide (- (card-width image) CARD-EDGE CARD-EDGE CARD-COIN-SIZE (/ CARD-COIN-SIZE .84) ))
	 (size (card-shrink-font-for-width xwide text CARD-TYPE-SIZE fontn))
	 (offset (if lineno (lineoff lineno) 0))
	 (bot (- (- CARD-BOTTOM-BAND offset))))
    ;;(message-string1 "card-set-type" offset bot)
    (card-make-text-layer image text 'center bot CENTER size fontn BLACK tweaks)))

(define (card-set-cost image cost . args)
  ;;(message-string1 "card-set-cost" image cost args)
  (let* ((size CARD-COIN-SIZE)
	 (rad (/ size 2))
	 (cx (+ CARD-EDGE 2 rad))
	 (cy (- cx)))
    (apply card-make-coin image cost cx cy size args) ; pass-thru if `(r180 #t)
    ))

;;; like card-set-cost, but transparent coin:
(define (card-set-step image step . args)
  (if msg-set-vp (message-string1 "card-set-step" image step args))
  (if (<= step 1) step			; do not show step = 1
      (let* ((size CARD-COIN-SIZE)
	     (rad (/ size 2))
	     (cx (- 0 CARD-EDGE 2 rad))
	     (cy cx))
	(set! args `((alpha 0) ,@args))
	(apply card-make-coin image step cx cy size args) ; just text, transparent coin/gold
	))
  )

(define msg-set-vp #f)
(define (card-set-vp image val . args)
  ;; args: 'size 'lead 'left 'color
  ;; (vp ('size 10) ('color BLACK) ...)
  ;;(gimp-context-set-background-color WHITE)
  (let* ((valu (if (number? val) (number->string val) val)) ; n
	 (font-size (util-assq 'size args CARD-VP-SIZE))
	 (ty (if (equal? valu "*") (* font-size .05) 0))
	 (top (util-assq 'lead args ty))
	 (left (util-assq 'left args 0))
	 (color (util-assq 'color args WHITE))
	 (fontn (util-assq 'font args CARD-VP-FONT))
	 ;;(font-size CARD-VP-SIZE)
	 (size (/ font-size .84))	; inferred radius
	 ;;(width (card-text-layer-width valu font-size fontn))
	 (x (- left (+ CARD-EDGE (/ size 2))))
	 (y (- top (+ CARD-BOTTOM-BAND 4)))
	 )
    (if msg-set-vp (message-string1 "card-set-vp" image val valu size fontn color))
    (card-make-text-layer image valu x y CENTER size fontn color nil))
  )


;; Dir -> 'C'hoose Direction; (save "D" for Debt)
(define ext-text `(("Base" "B") ("Dir" "C") ("Policy" "P") ("Event" "E") ("Event2" "E2")
		   ("Road" "R") ("High Tech" "HT") ("Transit" "T")))
(define self-ext `("Base" "Road" "High Tech" "Event" "Policy"))
(define (card-set-ext image val . tweaks)
  (if msg-set-vp (message-string1 "card-set-ext" image val tweaks))
  (if (member val self-ext) val		; don't show obvious Extension name
      (let* ((text (util-assoc val ext-text val))
	     (edge (* CARD-EDGE .8))	  ; in the un-safe area...
	     (size (* .3 CARD-COIN-SIZE)) ; tiny
	     (x (- 0 edge (/ size 2)))	  ; on the edge; should be (- 0 edge (/ text-width 2)))
	     (y (- 0 edge size)))	  ; as close as possible to bottom
	(message-string "card-set-ext" text x y)
	(apply card-make-text-layer-and-tweak image text x y CENTER size CARD-TEXT-FONT BLACK tweaks)))
  )
;;(define (card-make-text-layer-and-tweak image text x y justify size fontname color . tweaks))

(define (card-set-line image y color margin thick)
  ;;(message-string1 "card-set-line" image y color margin thick)
  ;; center horiz line at y; 
  (gimp-context-set-foreground (eval-sym color))
  (let* ((layer (card-base-layer image))
	 (mar (if (null? margin) 50 margin))
	 (size (if (null? thick) 4 thick))
	 (w (card-width image))
	 (x0 mar)
	 (x1 (- w mar))
	 (y0 (normalize-y image y))
	 (pts (vector x0 y0 x1 y0)))
    ;;(message-string1 "card-set-line pencil: " layer " #(" x0 y0 x1 y0 ")")
    (gimp-selection-none image)
    (gimp-context-set-brush-size size)
    (gimp-pencil layer 4 pts))
    (gimp-context-set-foreground BLACK)
  )

(define (card-white-region image raw?)
  ;; return (x0 y0 x1 y1)
  ;; from below title, price-bar
  ;; TODO: find the color at the spot: (gimp-drawable-get-pixel layer sx xy)
  (let* ((layer (card-base-layer image))
	 (sx CARD-EDGE)			; where to 'sample' for contiguous color
	 (sy (+ CARD-EDGE CARD-TOP-BAND CARD-COIN-BAND))
	 (k1 (gimp-image-select-contiguous-color image CHANNEL-OP-REPLACE layer sx sy))
	 (bounds (gimp-selection-bounds image))
	 (mar (if raw? 0 CARD-EDGE))	; raw? #t: full-image, #f: inset by CARD-EDGE
	 (x0 (+ (nth 1 bounds) mar))
	 (y0 (nth 2 bounds))
	 (x1 (- (nth 3 bounds) mar))
	 (y1 (nth 4 bounds))
	 (w (- (nth 3 bounds) (nth 1 bounds) mar mar))
	 (h (- (nth 4 bounds) (nth 2 bounds)))
	 )
    (gimp-selection-none image)
    (list x0 y0 x1 y1)))

(define (card-center left right width )
  ;; offset for something (the image|selection) of WIDTH to be centered between LEFT and RIGHT (card region)
  ;; or special case: (card-center 0 right width)
  (+ left (/ (- right left width) 2)))

(define msg-set-image #f)
(define (card-set-image image filename . args)
  ;;(message-string1 "card-set-image" image filename args)
  ;; name x y scale
  ;; default x y scale: center reduce to (min w-50, h-50)
  ;; open file as image, select all, copy content
  ;; (layer (gimp-file-load-layer RUN-NONINTERACTIVE image filepath))
  ;; (gimp-image-insert-layer image layer parent=0 pos=0 [top]) [or -1: above active layer]
  ;; new layer, paste, [scale], layer-set-offsets

  (define (layer-width layer) (car (gimp-drawable-width layer)))
  (define (layer-height layer) (car (gimp-drawable-height layer)))

  ;; set-title sets filename to "Title.png"
  (if (null? filename)
      (set! filename (image-basename image)))

  (let ((x (util-opt-arg args 'center))	; positional ASSERT (not (null? x))
	(y (util-opt-arg args 'center))	; positional ASSERT (not (null? y))
	(w (util-opt-arg args))
	(h (util-opt-arg args)))
    (if msg-set-image (message-string1 "card-set-image1:" `(card-set-image ,image ,filename ,@(list x y w h))))

    (let* ((filepath (string-append IMAGE-DIR filename))
	   (msg1 (if msg-set-image (message-string "card-set-image: filepath =" filepath)))
	   (layer (car (gimp-file-load-layer RUN-NONINTERACTIVE image filepath)))
	   (iw (layer-width layer))
	   (ih (layer-height layer))
	   (regc (gimp-selection-bounds image)); (list 0 0 (card-width image) (card-height image))
	   (reg0 (card-white-region image #f)) ; with CARD-EDGE margin (scale image into this region)
	   (reg1 (card-white-region image #t)) ; full image width
	   )
      (and msg-set-image (message-string "card-set-image2: layer=" layer "reg0:" reg0 "reg1:" reg1 "regc:" regc))
      (when (equal? x 'card)
	    (set! x (nth 1 regc))	; 0
	    (set! y (nth 2 regc))	; 0
	    (set! w (nth 3 regc))	; card-width
	    (set! h (nth 4 regc)))
      (when (equal? x 'reg)
	    (set! x (nth 0 reg1))	; 0
	    (set! y (nth 1 reg1))	; white part
	    (set! w (- (nth 2 reg1) x))
	    (set! h (- (nth 3 reg1) y)))
      (when (equal? x 'fit)
	    (set! x (nth 0 reg0))	 ; generally: CARD-EDGE
	    (set! w (- (nth 2 reg0) x))) ; scale width to fit
      (when (equal? y 'fit)
	    (set! y (nth 1 reg0))	 ; top of white
	    (set! h (- (nth 3 reg0) y))) ; scale height to fit
      (when (equal? y 'top)		 ; top 
	    (set! y (nth 1 reg0)))	 ; of white part
      
      (if msg-set-image (message-string "card-set-image3:" image layer (list x y w h) (list iw ih)))
      
      (gimp-image-insert-layer image layer 0 0)
      (gimp-item-set-name layer filename)
      (when (or (not (null? w)) (not (null? h))) ; re-scale image:
	    (let* ((w (if (null? w) iw w))
		   (h (if (null? h) ih (if (equal? h 'xs) (* ih (/ w iw)) h))))
	      (if msg-set-image (message-string "card-set-image4:" `(gimp-layer-scale ,layer ,w ,h ,TRUE)))
	      (gimp-layer-scale layer w h TRUE)))
      ;; 'center inside reg0: (inside CARD-EDGE and 'white' region) [similar to 'fit]
      (when (equal? x 'center)
      	    (set! x (card-center (nth 0 reg0) (nth 2 reg0) (layer-width layer)))) ; after scaling layer
      (when (equal? y 'center)
      	    (set! y (card-center  (nth 1 reg0)  (nth 3 reg0) (car (gimp-drawable-height layer))))) ; after scaling layer
      (if (not (null? x)) (set! x (normalize-x image x)))
      (if (not (null? y)) (set! y (normalize-y image y)))
      (if msg-set-image (message-string1 "card-set-image5:" image filename (list x y w h)))

      ;; push down to just above Background (behind text & other graphics)
      (gimp-image-lower-item-to-bottom image layer)
      (gimp-image-raise-item image layer)

      (gimp-layer-set-offsets layer (normalize-x image x) (normalize-y image y))
      layer))
  )

(define (card-set-fill image x y color)
  ;; bucket fill from given coords; suitable to apply team color
  ;;(message-string1 "card-set-fill" image x y color)
  (gimp-context-set-foreground (eval-sym color))
  ;;(gimp-selection-none image)
  (gimp-edit-bucket-fill (card-base-layer image)
			 BUCKET-FILL-FG
			 LAYER-MODE-NORMAL ; OVERWRITE?
			 100		   ; opacity
			 10		   ; use selection (low intensity... close matches?)
			 FALSE		   ; use layer, not composite
			 (normalize-x image x)
			 (normalize-y image y))
  (gimp-context-set-foreground BLACK)
  )

(define (card-set-price-bar image step stop rent . args)
  ;;(message-string1 "card-set-price-bar" image step stop rent)
  ;; assert card/image is portrait mode:
  (when (not (and (null? step) (null? stop) (null? rent)))
	(let* ((top (or (util-pop args) CARD-TOP-BAND))
	       (color (or (util-pop args) '(100 100 100))) ; light grey
	       (size (or (util-pop args) CARD-COIN-SIZE))
	       (p? #t)
	       (x 0) (y top) (w (template-width p?)) (h (+ size 10))
	       (cc (/ size 2))
	       (cy (+ y (/ h 2)))
	       (ce (+ CARD-EDGE 5 cc)))
	  ;;(card-select-edge-rect image x y w h)
	  (gimp-image-select-rectangle image CHANNEL-OP-REPLACE x y w h)
	  (card-set-fill image x y color )
	  (gimp-selection-none image)

	  (if (not (null? step)) (card-make-coin image step (+  ce) cy size))
	  (if (not (null? stop)) (card-make-coin image stop (/ w 2) cy size))
	  (if (not (null? rent)) (card-make-coin image rent (-  ce) cy size))
	  )))

(define (card-set-big-coin image valu)
  ;; Make big coin for ATM, Bank, etc.
  (let* ((h (- (card-height image) CARD-TOP-BAND CARD-COIN-BAND 10 10 CARD-BOTTOM-BAND ))
	 (cy (+  CARD-TOP-BAND CARD-COIN-BAND 10 (/ h 2))))
    (apply card-make-coin image valu 'center cy h `((oval .8))))
  )


(define msg-set-extras #f)
(define (card-set-extras image extras)
  ;;(message-string1 "card-set-extras" image extras)
  ;; extras: ( (key1 arg11 arg12) (key2 arg21 arg22) ...)
  (carloop extras (lambda (extra)
	 (let* ((key (car extra))
		(args (cdr extra)))
	   (if msg-set-extras (message-string1 "card-set-extra:" key args))
	   (case key
	     ((text)  (apply card-make-text-layer-and-tweak image args)) ; with tweaks
	     ((text-low) (apply card-set-text-low image args)) ; with tweaks
	     ((line)  (apply card-set-line image args))
	     ((image) (apply card-set-image image args))
	     ((coin)  (apply card-set-big-coin image args))
	     ((fill)  (apply card-set-fill image args))
	     ((step)  (apply card-set-step image args))
	     ((vp)    (apply card-set-vp image args))
	     ((ext)   (apply card-set-ext image args))
	     ((subtype) (apply card-set-type image (car args) '(lineno 1) (cdr args))) ; on second line
	     ((cardProps))		; nothing at this time
	     ((filen))			; nothing at this time
	     (else (message-string1 "Unknown Extra:" key args))
	     )))))

(define (card-set-page-name name . rest)
  ;; use name for future page(s), maybe force to new page
  ;; name eject
  ;; (message-string1 "card-set-page-name" name rest)
  (let ((eject (if (pair? rest) (car rest) #f))) ; (eject (util-opt-arg rest #f))
    (message-string1 "card-set-page-name" name "eject=" eject)
    (when eject
	  (save-if-template-full #t)	; save IFF not already saved
	  (set-end-of-template)		; mark template 'full' (or INVALID?) possibly leave empty slots
					; next card will begin new template/page
	  ))
  (para-set-global-pubname name)	; record 'name' in global parasite (parasites->card-global-pubname = name)
  (list #f name)			; NOT an new card image-layer
  )

(gimp-message "card define CARD generators")


;;; must do GIMP, to get card-width for back props
(define (card-type-back nreps name color portrait? ty by . extras)
  ;; make 2 bands of color:
  (let* ((text (util-opt-arg extras))	; optional text to write on back
	 (filen name)
	 (image-layer (card-make-base portrait? color ty by))
	 (image (car image-layer))
	 (layer (cadr image-layer)))
    (if text
	(let* ((size 80) (font CARD-COIN-FONT)
	       (height (card-text-layer-height text size font))
	       (cy (- (normalize-y image 'center) (/ height 2))))
	  (card-make-text-layer image text 'center cy CENTER size font BLACK nil)))
    (card-set-extras image extras)
    (message-string1 "card-back" image name color text)
    (gimp-image-set-filename image (card-filename-from-title name))
    ;; embed cost=card-width & step=card-height on Card with type="Back"
    ;; so Javascript can determine Card size before Image is loaded.
    (let ((nreps 1) (type "Back") (cost (card-scale (card-width image))) (step (card-scale (card-height image))))
      (card-write-info filen (syms-to-alist nreps type name cost step)))
    image-layer
    ))

;;; maybe from GTR? ensure card is filled with color to its original/current size
(define (card-add-bleed image neww newh width color)
  ;; resize canvas to (outx outy) & CENTER
  ;; select-rr
  ;; shrink by width
  ;; invert & fill w/color
  ;; select-rr, invert, delete
  (let* ((origw (card-width image))
	 (origh (card-height image))
	 (cx (/ (- neww origw) 2))
	 (cy (/ (- newh origh) 2))
	 (layer (card-base-layer image))
	 )
    (gimp-image-resize image neww newh cx cy)
    (gimp-layer-resize-to-image-size layer)
    (card-select-rr image)		; select NEW card-size/shape
    (gimp-selection-shrink image width)
    (gimp-selection-invert image)
    (gimp-context-set-foreground color)
    (gimp-drawable-edit-fill layer FILL-FOREGROUND)
    (card-select-rr image)
    (gimp-selection-invert image)
    (gimp-drawable-edit-fill layer FILL-TRANSPARENT)
    (gimp-context-set-foreground BLACK)
    ;; trim to original size:
    (gimp-image-resize image origw origh (- cx) (- cy))
    (gimp-layer-resize-to-image-size layer)
    (card-select-rr image)
    (gimp-selection-invert image)
    (gimp-drawable-edit-fill layer FILL-TRANSPARENT)
    (list image layer)))

(define DOT-EDGE (+ CARD-EDGE 20))	      ; inset by edge: l, r, t, b
(define CARD-DOT-SIZE 125)		      ; DOT for Move [525 - 100 = 425 / 3 = 141 * .88 = 125]
(define DOT-BAND #f)			      ; 123.5
(define DOT-XY #f)			      ; #(vector 7)

(define (square-portrait-bands card-size)
  ;; bands to leave square in middle of partrait image
  (let* ((tw (nth 0 card-size))		; 750 - 525 = 225 
	 (th (nth 1 card-size))
	 (cw (min tw th))
	 (ch (max tw th)))
    ;;(set! CARD-DOT-SIZE (* (/ (- cw DOT-EDGE DOT-EDGE) 3) .88)) ;; shrink from 1/3 of reduced width
    (ceiling (/ (- ch cw) 2))))

(define (get-dot-band)
  (or DOT-BAND (set! DOT-BAND (square-portrait-bands CARD-SIZE))))


(define (make-dot-xy dot-band dot-edge dot-size)
  ;; x-y location of each of 9 DOTS:
  (let* ((dot-s2 (/ dot-size 2))
	 (dot-dl (+ dot-edge 5 dot-s2))
	 (dot-dr (- dot-dl))
	 (dot-dc 'center)
	 (dot-dt (+ dot-edge 5 dot-s2 dot-band))
	 (dot-db (- dot-dt))
	 (dot-list `((,dot-dc ,dot-dc)	; element 0...
		     (,dot-dl ,dot-dt) (,dot-dc ,dot-dt) (,dot-dr ,dot-dt)
		     (,dot-dl ,dot-dc) (,dot-dc ,dot-dc) (,dot-dr ,dot-dc)
		     (,dot-dl ,dot-db) (,dot-dc ,dot-db) (,dot-dr ,dot-db))))
    (set! DOT-XY (list->vector dot-list))))

(define (card-type-move nreps name color value)
  ;;(message-string1 "card-type-move" name color value)
  ;; name is simply COLOR; uniqify it: (string-append colos "-" valus)

  ;; composition of each {0-6} value; index into DOT-XY:
  (define DOT-KEYS #(() (5) (1 9) (1 5 9) (1 3 7 9) (1 3 5 7 9) (1 3 4 6 7 9)))

  (let* ((colos (stringifyf color))
	 (valus (number->string value))
	 (type (if (zero? value) "Owner" "Distance"))
	 (filen (string-append type "-" colos "-" valus)) ; Distance-COLOR-n | Owner-COLOR-0
	 (title (string-append type " " valus))		  ; Distance n | Owner 0
	 (band  (get-dot-band)))

    (ifgimp
     (let* ((image-layer (card-make-base #t color band band))
	    (image (car image-layer))
	    (layer (cadr image-layer))
	    (dot-xy (or DOT-XY (make-dot-xy band DOT-EDGE CARD-DOT-SIZE)))
	    (dot-keys (vector-ref DOT-KEYS value)) ; '(1 9) for value 2
	    (make-dot-for-key
	     (lambda(key)		; ambient image; key is one of: {0 1 .. 9}
	       ;;(message-string1 "card-type-move" key dot-xy)
	       (let ((cxy (vector-ref dot-xy key)))
		 (apply card-make-dot image BLACK CARD-DOT-SIZE cxy)))))

       ;;(message-string1 "card-type-move" name value color image layer "band=" band)
       ;;(message-string1 "card-type-move" dot-keys make-dot-for-key)
       ;; non-standard title->filename

       ;; fill center [own] or write type [move]
       (if (zero? value)
	   (begin
	     (card-set-title image filen `(color ,color)) ; invisible ink
	     (card-set-fill image 'center 'center color)) ; 'own 0 
	   ;; else
	   (begin
	     (card-set-title image title `(filen ,filen)) ; "Move 3" "Move-PURPLE-3"
	     (card-set-type image type `(lineno .3)))     ; actual Move cards
	   )
       (for-each make-dot-for-key dot-keys)
       image-layer)
     (let ((cost value) (name filen) (color (eval-sym color)))
       (card-write-info filen (syms-to-alist nreps type name cost)))
     ))
  )

(define (card-type-dir nreps name . args)
  ;; Identify Direction Restrictions: name containing "[NESW]"
  (define (dir-bar image name block?)
    (define (edge-bar x y w h color)	; AMBIENT image
      (gimp-image-select-rectangle image CHANNEL-OP-REPLACE x y w h) ; card-select-edge?
      (card-set-fill image (+ x (/ w 2)) (+ y (/ h 2)) color))

    (let* ((del 35)
	   (mar 0)
	   (band (get-dot-band))
	   (bandx 0)
	   (bandy (+ band mar))
	   (cardw (card-width image))
	   (cardh (card-height image))
	   (xl (+ bandx 0))
	   (yt (+ bandy 0))
	   (xr (- cardw bandx del))
	   (yb (- cardh bandy del))
	   (barw (- cardw bandx bandx))
	   (barh (- cardh bandy bandy))
	   (c1 (if block? RED GREEN))
	   (c2 (if block? GREEN RED))
	   )
      (edge-bar xl yt barw del (if (string-contains name "N") c1 c2))
      (edge-bar xl yb barw del (if (string-contains name "S") c1 c2))
      (edge-bar xl yt del barh (if (string-contains name "W") c1 c2))
      (edge-bar xr yt del barh (if (string-contains name "E") c1 c2))
      ))
  ;; -------------------
  
  (let* ((type (util-opt-arg args "Direction")) ; or "Blocked"
	 (block? (not (equal? type "Direction")))
	 (band (get-dot-band))
	 (fontn DIR-TYPE-FONT)
	 (image-layer (card-make-base #t GREY band band))
	 (image (car image-layer))
	 (layer (cadr image-layer))
	 (cardw (- (card-width image) DOT-EDGE DOT-EDGE))
	 ;; shink to fit width:
	 (size (card-shrink-font-for-width cardw name 400 fontn))
	 ;;(msg (message-string1 "card-type-dir" name size))
	 (height (card-text-layer-height name size fontn))
	 (white-size (- (card-height image) band band))
	 ;; center y direction:
	 (ny (+ band (/ (- white-size height) 2)))
	 (nx 'center)
	 (filen (string-append type "-" name))
	 (ext (if (<= (string-length name) 1) "Base" "Dir"))
	 )

    (card-set-title image name `(filen ,filen)) ; override filename
    (card-make-text-layer image name nx ny CENTER size fontn BLACK nil)
    (dir-bar image name block?)
    (card-set-type image type `(lineno .3))
    (card-set-ext image ext)		; "Base" OR "Dir"
    (let ((name filen) (subtype name))
      (card-write-info filen (syms-to-alist nreps type name subtype)))
    image-layer)
  )

(define (card-type-auction nreps name cost1 cost2 color)
  ;; name mostly for mini-vec; maybe we could label the color slots...
  (let* ((portrait? #t)
	 (w (template-width portrait?))
	 (h (template-height portrait?))
	 (top (+ CARD-EDGE CARD-COIN-SIZE)) ; used in set-price-bar
	 (image-layer (card-make-base portrait? color 2 (- h top) ))
	 (image (car image-layer))
	 (layer (cadr image-layer))
	 (edge CARD-EDGE)
	 (r 10))
    (card-set-title image name `(color ,WHITE))
    ;;(card-set-price-bar image step stop rent . args)
    (card-set-price-bar image cost1 nil cost2 0 WHITE)
    ;; shrink bottom bar by edge:
    (gimp-image-select-round-rectangle
     image CHANNEL-OP-REPLACE edge (+ top edge) (- w edge edge) (- h top edge edge) r r)
    (gimp-drawable-edit-fill layer FILL-WHITE) ; save ink 
    image-layer)
  )

;; holds buffer-name when loaded; == (stringifyf (symbol-name))
(define TURN-R #f)
(define TURN-L #f)
(define THRU-S #f)
(define ROT-R #f)
(define ROT-L #f)

(define (card-image-load name)
  ;; insert an [IMAGE-DIR] image to card
  (let ((oname (string-append name ".png")))
    (car (gimp-file-load RUN-NONINTERACTIVE (string-append IMAGE-DIR oname) oname))))

(define (copy-to-named name)
  ;; return name of buffer containing name.png
  (let ((image (card-image-load name)))
    (gimp-selection-all image)
    (car (gimp-edit-named-copy (card-base-layer image) name)) ; return name
    ))

(macro (get-buffer-name form)		; (get-buffer-name R-TURN)
  ;; [load image, set SYM, copy to named buffer]
  ;; return image
  (let ((SYM (cadr form)))
    `(or ,SYM (set! ,SYM (copy-to-named (stringifyf ',SYM))))
    ))

(define msg-type-road #f)
(define (card-type-road nreps type name cost spec . args)
  ;; spec = List of (N E S W) :: (X L R S)

  ;; spec = List of ( C[R]0123 or S[R]0123 ...)
  ;; Get CR, C or S image, Rotate 90 x N {0..3}
  ;; R0 = N->W  L0 = N->E  S0 = N->S
  ;; L1 = E->S  R1 = E->N  S1 = E->W
  ;; from each point, go: Left (L) Rigth (R) or across S or block entry X
  ;; [N:X E:S S:E W:N] -> XSEN -> [X0 L1 R2 L3] [XLRL] #( ... (X L R L) ... )
  (define (buffer-name-for-code code)
    (case code
      ((L) (get-buffer-name TURN-L))
      ((R) (get-buffer-name TURN-R))
      ((S) (get-buffer-name THRU-S))
      ((X) (get-buffer-name THRU-X))
      ((RR) (get-buffer-name ROT-R))
      ((RL) (get-buffer-name ROT-L))
      (else (message-string "card-type road: invalid code=" (stringifyf code)))))

  (define (paste-buffer layer name)
    ;; return floating selection
    ;;(message-string1 "card-type-road:" name layer (stringify name))
    (car (gimp-edit-named-paste layer name TRUE)))

  (define (card-set-road-cost image cost . args)
    ;; raise coin up, because smaller BASE region
    (let* ((size CARD-COIN-SIZE)
	   (rad (/ size 2))
	   (cx (+ CARD-EDGE 2 rad))
	   (cy (+ (- cx) 5)))				; 5 pixels north
      (apply card-make-coin image cost cx cy size args) ; pass-thru if `(r180 #t)
      ))

  ;; --------------------------
  (if msg-type-road (message-string1 "card-type-road:" 'spec= spec 'args= args))
  
  (let* ((align? (> (string-length (symbol->string (car spec))) 1)) ; "RR" or "RL"
	 (symet? (and (eq? (nth 0 spec) (nth 2 spec)) (eq? (nth 1 spec) (nth 3 spec))))
	 (rotate (and (not symet?) (not align?)))
	 ;;(msg1 (message-string1 "card-type-road:" 'align?= align? 'symet?= symet? 'rotate= rotate))
	 (cprops (cardprops args))
	 (ooStep (util-assq 'onStep cprops))
	 (onStep (if (pair? ooStep) (list ooStep) ()))
	 (oprops (list '(noStop #t)))	     ; cardProps after splicing out onStep TODO!
	 (roadDir (list->vector (map symbol->string spec)))
	 (action (if align?
		     `((align ,roadDir))
		     `((cardFields "roadSpec") (roadSpec (set ,roadDir))
		       (onStep (dist (add 1)) ,@onStep (moveDir (roadDir roadSpec))))))
	 ;;(msg (message-string1 "card-type-road:" 'action= action 'roadDir= roadDir))
	 (subtype (if align? nil "Transit")) ; (subtype (if rotc? "rotate" "symetric"))
	 (color TRANSIT-COLOR)		     ; very light grey
	 (filen name)			     ; generic, no tweaks
	 ;;(message-string1 "card-type-road" subtype filen)
	 (band (get-dot-band))
	 ;;(band 120)
	 )
    (ifgimp
     (let* ((image-layer (card-make-base #t color band band))
	    (image (car image-layer))
	    (layer (cadr image-layer))
	    (n -1))
       (card-set-title image name `(color ,WHITE))
       (card-set-extras image `((ext "Road")))
       ;;(card-set-extras image `((subtype ,subtype (color ,WHITE)))) ; idenfify as Transit
       (if (not (null? cost))
	   (card-set-road-cost image cost))

       ;; this rotated "cost" so it's easy to find "total build cost" even when card is rotated
       ;; and a visual indication that card is rotatable
       (if rotate
	   (apply card-set-road-cost image cost `((r180 #t))))
       (apply card-set-type image type `((color ,WHITE) (lineno .3)))
       (for-each (lambda(s)
		   (let* ((float-sel (paste-buffer layer (buffer-name-for-code s))))
		     (if (> n -1) (gimp-item-transform-rotate-simple float-sel n TRUE 0 0))
		     (gimp-floating-sel-anchor float-sel)
		     (set! n (+ 1 n))))
		 spec)
       image-layer)
     (let* ((ext    "Roads")
	    (props `(,@action ,@oprops)))
       (card-write-info filen (syms-to-alist nreps type name cost subtype ext props)))
     )))


(define (card-generic portrait type title color cost text . tweak)
  ;;(message-string1 "card-generic" portrait type color title cost text)
  (let* ((filen (util-assq 'filen tweak title))
	 (image-layer (card-make-base portrait color CARD-TOP-BAND CARD-BOTTOM-BAND))
	 (image (car image-layer))
	 (layer (cadr image-layer)))
    ;;(message-string1 "card-generic" title filen)
    (card-set-title image title `(filen ,filen))
    (card-set-type image type)
    (if (not (null? cost)) (card-set-cost image cost))
    (card-set-text image text)		; may be NO-OP
    image-layer))

;; generate filename by appending title-xname-type
;; because Event & Policy may have multiple cards with same title.
(define (card-xname title types extras)
  (let* ((xname (util-assq 'xname extras ""))
	 (typen (if (equal? types "Policy") (string-append "-" types) "")))
    (string-append title xname typen)))

(define msg-type-event #f)
(define (card-type-event nreps type title color text . extras) ; [cost text2 ((ext ?) (step ?) ...)]
  (if msg-type-event (message-string1 "card-type-event" type title color extras))
  (let* ((cost	(util-opt-arg extras nil)) ; positional arg 
	 (text2 (util-opt-arg extras nil)) ; positional arg 
	 (ext   (util-assq 'ext extras (if (equal? type "Event") "Event" "Policy")))
	 (step  (util-assq 'step extras 1))
	 (filen (card-xname title type extras)) ; filen-type
	 (subtype (util-assq 'subtype extras nil)))
    (set! extras `((ext ,ext) (step ,step) ,@extras))
    (if msg-type-event (message-string1 "card-type-event" type title color cost text text2 extras))

    (ifgimp
     (let* ((image-layer (card-generic #f type title color cost text `(filen ,filen)))
	    (image (car image-layer))
	    (layer (cadr image-layer)))
       (card-set-text-low image text2)
       (card-set-extras image extras) 	; set text, text-low, coin, step, vp...
       ;;(set!-eval-sym color)
       image-layer)
     (let ((name title) (props (cardprops extras)))
       (card-write-info filen (syms-to-alist nreps type name cost step subtype ext props)))
     )))

(define msg-type-tile #f)
;;(12 residential [BROWN] "Residential" "Housing" nil nil "*" 2 "Cost VP Rent\n2 House 1   0" )
(define (card-type-tile nreps type title color cost step stop rent text . extras)
  (if msg-type-tile (message-string1 "card-type-tile" type color title cost step stop rent text extras))
  (let* ((filen (util-assq 'filen extras title))
	 (ext   (util-assq 'ext extras "Base"))
	 (subtype (util-assq 'subtype extras nil))
	 (vp (util-assq 'vp extras nil)))
    (set! extras `((ext ,ext) (step, step) ,@extras))
    (if (equal? subtype "Transit") (set! color TRANSIT-COLOR))
    (if (equal? subtype "Com-Transit") (set! color COM-TRANSIT-COLOR))

    (ifgimp
     (let* ((image-layer (card-generic #t type title color cost () `(filen ,filen)))
	    (image (car image-layer))
	    (layer (cadr image-layer)))
       (card-set-price-bar image step stop rent) ; if applicable
       (card-set-text image text)
       (card-set-extras image extras)
       image-layer)
     (let ((name title) (props (cardprops extras))) ;  (step step)
       (card-write-info filen (syms-to-alist nreps type name cost step stop rent vp subtype ext props)))
     )))
    
(define msg-type-home #f)
(define (card-type-home nreps type title color cost step stop rent bgcolor)
  (if msg-type-home (message-string1 "card-type-home:" type title color cost step stop rent bgcolor))
  (let* ((text ())
	 (colos (stringifyf bgcolor))	       ; assert bgcolor is SYMBOL: 'RED -> "RED"
	 ;; convert GIMP color to CSS color in stringify
	 (rgbColor (cons 'rgb (eval bgcolor))) ; 'RED -> '(rgb 239 32 60) -> JSONify -> "rgb(239,32,60)"
	 (vp 1)
	 (filen (string-append "Home-" colos)))
    (ifgimp
     (let* ((image-layer (card-generic #t type title color cost () `(filen ,filen)))
	   (image (car image-layer))
	   (layer (cadr image-layer)))
       (card-set-price-bar image step stop rent)
       (card-set-fill image 60 260 bgcolor)
       (card-set-image image "Home.png" 86 238 360 371) ; filename x y new-width new-height
       (card-set-extras image `((vp ,vp)))
       image-layer)
     (let* ((name filen) (ext "Base") (props `((rgbColor ,rgbColor))) (subtype "Home"))
       (card-write-info filen (syms-to-alist nreps type name cost step stop rent vp subtype ext props)))
     )))


;; (12 cirlce "House" "house"	2 () () 0 () (vp  1) (image "House0.png") (cardProps (noStop #t)))
;; (1  circle "Debt"  "Debt"   () () () () () (cardProps (noStop #t)))
(define msg-type-circle #f)
(define (card-type-circle nreps title type color cost step stop rent text . extras)
  ;; note that args are TITLE(name) *then* TYPE(types) 
  (if msg-type-circle (message-string1 "card-type-circle:" type title color cost step stop rent text extras)) 
  ;; use cost to print the cost?
  (let* ((filen (util-assq 'filen extras title))
	 (colos (symbol->string color))	; assert bgcolor is SYMBOL: 'RED -> "RED"
	 ;; convert GIMP color to CSS color in stringify
	 (size CIRCLE-IMAGE-SIZE)	     ; 125/scale -> 250?
	 (radius (/ size 2))		     ; "rounded rectangle" is a circle
	 (subtype (util-assq 'subtype extras nil))
	 (vp (util-assq 'vp extras nil)))
    (if msg-type-circle (message-string1 "card-type-circle: PRE-GIMP" filen type title))
    (ifgimp
     (let* ((image-layer (card-make-base-image size size radius )) ; 62.5 x 62.5 in final bitmap
	    (image (car image-layer))
	    (layer (cadr image-layer)))
       (if msg-type-circle (message-string1 "card-type-circle: GIMP" 'image-layer= image-layer filen type title))
       (gimp-image-set-filename image (card-filename-from-title filen))
       (gimp-selection-none image)
       (card-set-fill image radius radius color)
       (card-set-extras image extras)	; (card-set-image image "House0")
       (gimp-image-scale image 125 125)
       image-layer)
     (if msg-type-circle (message-string1 "card-type-circle:" 'image-layer= image-layer filen type title))
     (let ((name filen) (props `((noStop #t))))
       (card-write-info filen (syms-to-alist nreps type name cost step stop rent vp subtype props)))
     )
    ))

(define (card-get-nreps image)
  ;; used by sf-to-template-nreps
  (catch (lambda(err) 1)		; don't print "gimp-image-get-parasite failed"
	 (string->number (nth 2 (car (gimp-image-get-parasite image "nreps"))))))

(define (card-set-nreps image nreps)
  ;; record nreps for future... (sf-to-template-nreps
  (gimp-image-attach-parasite image (list "nreps" 1 (number->string nreps))))

(define (card-do-template-nreps image nreps undo context)
  ;; put nreps copies of card image onto template (also: sf-to-template-nreps)
  (catch
   (begin (message-string "card-do-template-nreps catch" CARD-LOOP "nreps" nreps) #t)
   (while (and (> nreps 0) CARD-LOOP)
	  ;; use script-fu-card-template.scm
	  (card-to-template image (card-base-layer image) undo context) ; and save-if-template-full
	  (set! nreps (- nreps 1)))))  

(define (displayed? image)
  ;; #t if image has parasite "card-display"
  (pair? (catch nil (gimp-image-get-parasite image "card-display"))))

(define (card-put-image image nreps display? template?)
  ;; either display this card, OR push to template (nreps times)
  (message-string "card-put-image:" image nreps "display? =" display? "DO-TEMPLATE =" template?)
  (card-set-nreps image nreps)
  (if (and display? (not (displayed? image)))
      (let ((display (car (gimp-display-new image)))) ; create display of image
	;; 'Parasite' (record 'display' on image so we can find/remove it later)
	(para-set-display image display) 
	(gimp-displays-flush)))
  (if template? (card-do-template-nreps image nreps #f #t)) ; gimp-edit-copy/paste, rotate, set-offsets
  )

;;; Aux applications will override with calls to make-base-card-image, etc.
(define (card-aux-proc text . args)
  (message-string1 "original card-aux-proc: " AUX-PROC)
  (if AUX-PROC
      (begin
	(template-use (nth 0 AUX-SPEC) (nth 1 AUX-SPEC)) ; try again here...
	(message-string1 "card-aux-proc1: " (stringify (template-get-spec)))
	(apply AUX-PROC text args)
	)
      '(#f "aux")))

;;; (cdr (assq 'cardProps extras)) returns an alist: ((key val) (key1 val1)...)
(define (cardprops extras)
  ;; find cardProps if it was supplied:
  (if (pair? extras)
      (let ((props (assq 'cardProps extras)))
	(if (not props) nil (cdr props))) ; TODO: this should be (cadr props)!!
      nil))

;;; return image-layer
(define msg-make-one #f)
(define (card-make-one-card card-spec)
  (message-string1 "card-make-one-card" (stringify card-spec))
  ;; card-spec: (nreps 'type "title" ... )
  ;; nreps road name text spec
  ;; nreps event name color text 
  ;; nreps tile name args
  (define (type-string type)
    (let ((alist `((home "Residential") (res "Residential") (com "Commercial") (fin "Financial")
		   (ind "Industrial") (mun "Municipal") (gov "Government")
		   (deferred "Deferred") (event "Event") (future "Future Event") (temp "Temp Policy")
		   (policy "Policy") (road "Road"))))
      (util-assq type alist (symbol->string type))))

  (define (make-card-image card-spec)
    (and msg-make-one (message-string1 "make-card-image" (stringify card-spec)))
    ;; all specs for TILES that have PROPS have: nreps type name . args
    (let* ((nreps (abs (nth 0 card-spec))) ; (< (nth 0 card-spec) 0) for non-templated 'cards'
           (type (nth 1 card-spec)) (types (type-string type))
           (name (nth 2 card-spec))
           (args (cdddr card-spec))     ; (nthcdr 3 card-spec) == (rest)
           ;;
           ;; (msg (message-string1 "card-make-one-card" 'nreps= nreps 'types= types 'name= name 'args= args))
           (image-layer
            (case type
              ;; tiles:
              ((home)       (apply card-type-home nreps types name BROWN args))
              ((circle)     (apply card-type-circle nreps name args)) ; name types COLOR args [not types="circle"]
              ((res)        (apply card-type-tile nreps types name GREEN args))
              ((com)        (apply card-type-tile nreps types name BROWN args))
              ((fin)        (apply card-type-tile nreps types name BROWN args))
              ((ind)        (apply card-type-tile nreps types name BROWN args))
              ((mun)        (apply card-type-tile nreps types name BROWN args))
              ((gov)        (apply card-type-tile nreps types name PURPLE args))
              ;; events & policy: args = color title text [cost] [text-below]
              ((deferred)   (apply card-type-event nreps types name args))
              ((event)      (apply card-type-event nreps types name args))
              ((future)     (apply card-type-event nreps types name args))
              ((temp)       (apply card-type-event nreps types name args))
              ((policy)     (apply card-type-event nreps types name args))
              ;;
              ((road)       (apply card-type-road nreps types name args))

              ;; DOTS: "NAME-value" COLOR value
              ((mov)        (apply card-type-move nreps name args))
              ((own)        (apply card-type-move nreps name args))
              ((dir)        (apply card-type-dir nreps name args))
              ;;
              ((auction)    (apply card-type-auction nreps name args))
              ((back)       (apply card-type-back nreps name args)) ; with (nreps 1)
	      ((skip)	    (next-slot-on-template nreps)) ; leave a blank slot [but: backfill-ilxy]
              ;;
              ((aux)        (apply card-aux-proc nreps name args)) ; special case for external usage.
              ;; DIRECTIVES: (no image, no layer)
              ((page)       `(#f 0))    ; never happens: handled by card-make-deck: do-page
              ((deck)       (message-string "MAKE DECK:" args) `(#f 0)) ; ignore, spec0 used by card-deck-name
              (else (message-string1 "unknown type=" type args) '(#f 0))
              ))
           (image (car image-layer))    ; image or #f
           (layer (cadr image-layer))	; layer or page-name
           )
      (and msg-make-one (message-string "make-card-image: MADE" image layer))
      (when image			; from (page) or (deck) or (not DO-GIMP)
	    (and msg-make-one (message-string1 "try card-put-image" image nreps CARD-DISPLAY? DO-TEMPLATE))
	    ;; put full scale image to display and/or template 
	    (card-put-image image nreps CARD-DISPLAY? DO-TEMPLATE) ; maybe show card-image on DISPLAY & DO-TEMPLATE
            (let ((filename (car (gimp-image-get-filename image))))
              (and msg-make-one (message-string1 "try merge-and-save" filename))
              (when (and (string? filename) (> (string-length filename) 0) CARD-SAVE-PNG)
		    (when (equal? (card-width image) (car CARD-SIZE)) ; indicates unscaled
			  (let ((w (card-scale (card-width image)))
				(h (card-scale (card-height image))))
			    (and msg-make-one (message-string1 "try scale image" w h))
			    (gimp-image-scale image w h)))
		    ;; save at current scale:
		    (and msg-make-one (message-string1 "try file-merge-and-save" image))
		    (file-merge-and-save image)))
            )
      image-layer)			; or (#f pagename)
    )
  ;; if      (DO-TEMPLATE && (< n 0)) ==> dont make image
  ;; if (not (DO-TEMPLATE && (< n 0))) => do make-image
  ;; ZERO REPS used for directives (page, deck)
  (if (not (and DO-TEMPLATE (< (nth 0 card-spec) 0))) ; (< n 0) indicates not suitable for TEMPLATE
      (make-card-image card-spec)
      )
  )

;; (for-each proc (vector->list deck))
(define (do-spec-vector deck proc)
  (let* ((line 0) (nlines (vector-length deck)))
    (catch
     (begin (message-string "do-spec-vector catch" CARD-LOOP "line" line "of" nlines) #t)
     (while (and (< line nlines) CARD-LOOP)
	    (proc (vector-ref deck line))
	    (set! line (+ 1 line))))))

(define (card-make-deck deck)
  ;; deck is array of array #(n, title, filename, type, ep stop rent cost text1 . extra
  ;; see: card-set-extras for capabilities of extra
  (message-string "card-make-deck: n=" (vector-length deck))

  (and context (gimp-context-push))	; will be changing foreground color and brush-size
  (and context (gimp-context-set-defaults))
  ;; BLACK on WHITE

  (let ((named? #f))
    (define (do-page spec)
      (message-string1 "Page detected: " spec) ; (0 page "Name" #t)
      (apply card-set-page-name (cddr spec)) ; (name) OR (name #t) to eject
      (set! named? #t))
    
    ;;  Each "spec" element: (nreps type name . args) For ex: (0 page "Coins" #t)
    (define (do-line spec)
      ;;(message-string1 "card-make-deck: spec=" spec)
      (if (equal? (nth 1 spec) 'page)
	      (do-page spec)
        (card-make-one-card spec)))

    ;; invoke do-line on each element of deck vector: either card-make-one-card OR do-page
    (do-spec-vector deck do-line)
    
    (if named? (save-if-template-full #t))) ; ensure partial page is exported. (maybe backfill and re-export!)
  (and context (gimp-context-pop))
  )

(define (card-deck-name deck)
  ;; get (0 deck "ClassName" "file-name") if it exists as element 0 of deck
  (let* ((length (vector-length deck))
	 (spec0 (if (> length 0) (vector-ref deck 0) #f)))
    (message-string1 "card-deck-name" spec0)
    ;; (apply (lambda(nreps typen class fname) `(,class ,fname)) spec0)
    (if (or (not spec0) (not (equal? 'deck (cadr spec0)))) `(#f #f)
	(let* ((nreps (util-pop spec0)) ; 0
	       (typen (util-pop spec0))	; 'deck
	       (class (util-pop spec0))	; "CamelName"
	       (fname (util-pop spec0))	; "hyph-name" [.ts]
	       )
	  `(,class ,fname)))))

(define (card-make-deck-with-file deck)
  ;; make a typescript file if deck has [file-name] element.
  ;; else simply card-make-deck
  ;; (0 deck ClassName [file-name])
  (message-string1 "card-make-deck-with-file" (vector-length deck))

  (let* ((class-file (card-deck-name deck))
	 (class (util-pop class-file))
	 (filen (util-pop class-file))) ; or #f

    ;; binding deck, class, filen:
    (define (deck-to-file)
      (deck-write-intro class)
      (card-make-deck deck)
      (deck-write-final class))		; #t if success

    (message-string1 "card-make-deck-with-file" class filen DO-INFO)
    (if (or (not filen) (not DO-INFO))
	(card-make-deck deck)		; no typescript/info, no display/output

	;; card-write-info with-output-to-file-catch:
	(let* ((dirname (string-append BASE-DIR PROJECT-DIR "/cardinfo/"))
	       (filename (string-append dirname filen ".ts")))
	  (message-string1 "write CardInfo(deck) to file" class filename)
	  (if (with-output-to-file-catch #f filename deck-to-file)
	      (message-string1 "deck-to-file: DONE" class filename)
	      (message-string1 "deck-to-file: FAILED" class filename)))
	)))

;;; (sym1 sym2 ...) => `((sym1 ,sym1) (sym2 ,sym2) ...)
;;; ==> (list (list 'sym1 sym1) (list 'sym2 sym2) ...)
;;; create list macro form, to be evaluated in context:
(macro (syms-to-alist form)
  (define (syms-to-pairs syms)
    (if (pair? syms)
	(let ((sym (car syms)))
	  `((list ',sym ,sym) ,@(syms-to-pairs (cdr syms) )))
	))
  `(list ,@(syms-to-pairs (cdr form))))

(define (deck-write-intro class)
  (display "import { Deck } from '../card'\n")
  (display "import { C } from '../basic-intfs'\n")
  (display "/** This class and file are auto-generated do not edit. */\n")
  (display (string-append "export class " class " {\n"))
  (display (string-append "   static deck: Deck = {\n      name: \'" class "\',\n      cards: [\n")))

(define (image-basename image)
  (util-file-basename (car (gimp-image-get-filename image))))

;; remove (key value) from (props (k v) (k1 v1)), since (k v)is now in alist.
;; must store the return value back to (cdr props)
(define (remprop key plist nlist)
  (if (null? nlist) (set! nlist (cons #t plist)))
  (if (and (pair? plist) (list? plist))
      (if (equal? key (caar plist)) (set-cdr! nlist (cdr plist))
	  (remprop key (cdr plist) plist)))
  (cdr nlist)
  )


(define msg-write-info #f)
(define (card-write-info filen alist)
  ;; alist: cost, step, stop, rent, vp, ... props
  ;; write card-info to current open file:
  ;; supply valid filen OR alist with ('name 'type ['xname] )
  (and msg-write-info (message-string1 "card-write-info" filen alist DO-INFO))

  ;; [assoc-pair] true if pair has a non-null cdr: '(key val) [exclude: '(key)
  (define (apair? x) (and (pair? x) (not (null? (cdr x)))))

  ;; so typescript can use type: number (rather than: number | string)
  (define (one-prop key alist)
    (let* ((apair (assq key alist)))	     ; '(key aval)
      (if (apair? apair)		     ; key is defined in alist
	  ;; (and (not (number? (cadr apair))))
	  (let* ((aval (cadr apair))	     ; assigned value in alist
		 ;; look for overriding val in 'props'
		 (props (assq 'props alist)) ; '(props ((a 1) (b 2))) '(props ()) '(props) #f ==> ((a 1)(b 2)) () nil
		 (msg0 (and msg-write-info (message-string "props" props)))
		 (plist (if (apair? props)	      ; is there a proper (props plist)?  
			    (cadr props) #f))	      ; extract plist ((k0 v0)(k1 v1)...) 
		 (kprop (if (list? plist)	      ; [check for malformed plist...?
			    (assq key plist) #f))     ; kprop =  '(rent 0)  '(a 1)
		 (nval (if (apair? kprop)	      ; is there a key override?
			   (cadr  kprop) (if (null? aval) 0 aval)))
		 (msg1 (and msg-write-info (message-string "apair" apair "props" props "plist" plist "aval" aval "kprop" kprop "nval" nval)))
		 )
	    (set-cdr! apair (list nval))
	    (and msg-write-info (message-string "remprops" key "plist" plist "props" props))
	    (if plist (set-cdr! props (list (remprop key plist nil))))
	    ))
      )
    alist)

  ;; test:
  ;; (define blist1 `((cost 1) (step nil) (stop "*4*") (rent 1) (vp nil) ))
  ;; (define blist2 `((cost 1) (step nil) (stop "*4*") (rent 1) (vp nil) (props ())))
  ;; (define blist3 `((cost 1) (step nil) (stop "*4*") (rent 1) (vp nil) (props ((stop 4)))))
  ;; (define blist4 `((cost 1) (step nil) (stop "*4*") (rent 1) (vp nil) (props ((stop 4) (step 3)))))
  ;; (define blist5 `((cost 1) (step nil) (stop "*4*") (rent 1) (vp nil) (props ((stop 4) (step 3)))))
  ;; (one-prop 'stop blist1)
  ;; (one-prop 'stop blist2)
  ;; (one-prop 'stop blist3)
  ;; (one-prop 'stop blist4)
  ;; (one-prop 'step blist5)

  (if DO-INFO				; for online: write line to typescript class info file:
      (let* ((path (path-to-image filen alist))
	     (blist (append alist `((path ,path)))))
	(one-prop 'step blist)		; 'cost' ok to null, esp for Policy/Event
	(one-prop 'stop blist)		; 'vp' ok to null/string 
	(one-prop 'rent blist)
	(and msg-write-info (message-string "card-write-info: blist:" blist))
	(catch (message-string "card-write-info: catch")
	       (let ((json     (stringify blist 'JSON)))
		 (message-string "card-write-info: JSON-ified:" json "\n")
		 (display (string-append "        " json ",\n")))
	       (and msg-write-info (message-string "card-write-info: success" path))
	       )
	)))

;;; image is the object-id from GIMP, or 0 when there is no image
;;; for gimp-image-set-filename (the only tweak of interest is `(filen filen), so we just use filen

(define (path-to-image filen alist)
  ;;(message-string1 "path-to-image" filen alist)
  (if filen
      (util-file-basename (card-filename-from-title filen)) ; dir/filen.png => "filen.png" (image-basename image)
      (let* ((xname (card-xname (util-assq 'name alist) (util-assq 'type alist) alist))) ; title-xname-type
	(util-file-basename (card-filename-from-title xname)))) ; dir/xname.png => "xname.png"
  )

(define (deck-write-final class)
  (display "   ]\n  }}\n"))


;;; to cleanup: (replace-regexp '([^\])"' '\1') (regexp-replace '\"' '"')
;;; [^\] will resolve to start-of-line, or { or }
;;; IF a " should survive, it will have been slashified!
;;; (replace-regexp "\\([^\\\\]\\)\"" "\\1")
;;; (replace-regexp "\\\\\"" "\"")
;;; (replace-string "\\n" "\\C-j")

(gimp-message "card define DECK vectors")

(define (expandify deck)
  ;; "expand top-level expressions in given DECK: replace 1-to-1 with (eval elt)"
  (define (ifelt ndx val)
    (if (not (number? (car val)))	; if nreps is a function-name!
	(vector-set! deck ndx (eval val))))
  (while-vector deck #f ifelt)
  deck)


(define (find-card deck name)
  ;; EVENT & POLICY have COLOR symbol in slot 2, name in slot 3
  (let* ((n 0) (element nil))
    (while (and (< n (vector-length deck)) (null? element))
	   (let* ((vn (vector-ref deck n))
		  (e2 (nth 2 vn))
		  (ename (if (symbol? e2) (nth 3 vn) e2)))
	     (when (equal? ename name)
		   (set! element vn)))
	   (set! n (+ 1 n)))
    element))

;;; nreps: 0 -> copy from original; >0 -> set nreps
(define (mini-vec nreps deck . names)
  (let ((vec (make-vector (length names))) (ndx 0))
    (for-each (lambda (name)
		(let* ((spec1 (find-card deck name))
		       (spec2 (if (pair? spec1) (list->vector spec1) (make-vector 1))))
		  (if (> nreps 0)
		      (vector-set! spec2 0 nreps))
		  (vector-set! vec ndx (vector->list spec2)))
		(set! ndx (+ 1 ndx)))
	      names)
    vec))

;;; expandify to insert selected row vector-elt from selected DECK
(define (mini-vec0 nreps deck name)
  (let ((v0 (vector-ref (mini-vec nreps deck name) 0)))
    `(,nreps ,@(cdr v0))))

(define BACK-DECK
  #((0 deck "BackDeck" "back-deck")
    (0 page "CityBack" #t)						  ; eject to new page
    (18 back "City-Back" GREY #t 60 60 "" (image "CitymapBack.png" card)) ; cityback
    (18 back "Tile Back" GREY #t 60 60 "" (image "CitymapBack.png" card)) ; cityback
    (18 back "Event Back" GREY #f 60 60 "" (image "CitymapBackL.png" card)) ; cityback
    ))
(define EVENT-BACK
  #((0 deck "EventBack")
    (0 page "EventBack")
    (36 back "Event Back" GREY #f 60 60 "" (image "CitymapBackL.png" card)) ; cityback
    ))

(define HOME-BACK
  (expandify
   #((0 deck "HomeBack")
     (3 one "RED" RED 0)
     (mini-vec0 1 BACK-DECK "City-Back")
     (3 one "BLUE" BLUE 0)
     (mini-vec0 1 BACK-DECK "City-Back")
     (4 own "BLACK" BLACK 0)
     (mini-vec0 6 BACK-DECK "City-Back")
     )))

(define TOKEN-DECK
  ;; never goes to template (nreps < 0)
  (expandify
   #((0 deck "TokenDeck" "token-deck")	; name of "Class" and "file" in TS
     ;; n   type title cost ep stop rent text1 line text2 image
     ;;    type   name         types  color cost... vp text
     (-32 circle "Debt"       "Debt"  DEBT   1 () () 0 () ) ;(image "Debt.png"))

     (-12 circle "House"      "house" GREEN  2 () () 0.5 () (vp 1) (image "House0.png"))
     (-12 circle "Triplex"    "house" GREEN  5 () () 1 () (vp  3) (image "House1.png"))
     (-12 circle "Apartments" "house" GREEN  8 () () 2 () (vp  6) (image "House2.png"))
     (-12 circle "High Rise"  "house" GREEN 11 () () 4 () (vp 10) (image "House3.png"))
     (-12 circle "Tower"      "house" GREEN 14 () () 8 () (vp 15) (image "House4.png"))
     (mini-vec0 0 BACK-DECK "City-Back")
     )))

(define HOME-DECK			; type-home
  (expandify
   #((0 deck "HomeDeck" "home-deck")	; name of "Class" and "file" in TS
     (0 page "Homes")

     (1 home "Home"    0  1 0 1 RED)
     (3 own "RED" RED 0)

     (1 home "Home"    0  1 0 1 BLUE)
     (3 own "BLUE" BLUE 0)

     (1 home "Home"    0  1 0 1 GREEN)
     (3 own "GREEN" GREEN 0)

     (4 own "BLACK" BLACK 0) 		; VC-Tokens & no_Rent

     ;;(0 page "HomeBack" #t)		; page-eject, make back on clean sheet:
     ;;(1 own "BLUE" BLUE 0)
     ;;(1 own "RED" RED 0)

     ;;(0 page "Owners" )	    ; Note: Owners is its own back....
     ;; Pro'ly want to put solid color on both sides to make owner Tokens

     (mini-vec0 0 BACK-DECK "City-Back")
     )))


;; cardProps grammar: JSON == KVlist
;; Tiles: {onBuild, onDiscard, untilDiscard, onStep, onStop, onMove, whileOn} {cost, step, stop, rent, vp}
;; ---->  (coins_add, moves_add, draws_add, builds_add, buys_add, dist_add, buildAdjust_add, 
;; there is a Table & Player slot for "buildAdjust" which is included when configBuildCost
;; the "builds" field is the "buildCount" and is reset to 1 [plus turnBuilds] each Turn

;; Cards will post their effects to the Effects database.
;; Code that is affected by the effect will grep for applicable DataRecs


;; Action: {target:object/class, slot:string, verb:[add,set], value:number, filter...}

;; calcRent: find all (rentAdjust...) effects for this Player or Tile (filter range sub/type)

;; untilDraws:  removed by player.draw() [where player.draws -= 1]
;; untilBuilds: removed by player.build() [where player.moves -= 1]


;; Simple effects: Check Card.props for onMove, onStep, onStop.
;; onStep: 
;; onStop: {field: { mods }}  (onStop: { buys: {add 1}})
;; onMove: {field: { mods }}  (onMove: { dist: {add -1}}) 

;; Event:


(define msg-high-tech #f)
(define (high-tech name subtype)
  (let* ((match (if (equal? name "High Tech") 'name 'subtype))
	 (filter `(,match "High Tech"))
	 (filen (if (equal? name "High Tech") `((filen ,(string-append name " " subtype))) nil)))
    (if msg-high-tech (message-string1 "high-tech" name subtype filter filen))
    `(1 ind ,name    "9+" 2 "*3*" "2*" "+1 Value Token\n(max 6)\n\n* + $1 per Value Token"
	(vp 1) (subtype ,subtype)
	(text-low "+ $1 Rent to Com/Res in Range:2\n+ $2 Cost for other High Tech")
	,@filen
	(ext "High Tech")
	(cardProps
	 (stop 3) (rent 2) (cost 9)
	 (cardFields #("valueToken" "valueTokenCounter" "stopCounter"))
	 (valueToken 0)
	 (valueTokenCounter (counter #("Value" 93 -20)))
	 (stopCounter (counter #(0 0 -104.5 ,C.coinGold))) ;; 0->no_label;  dy = -187.5 + 83 = -104.5
	 (onStep (valueToken (add 1) (max 6))
		 (rent (set 2) (add valueToken))
		 (stop (set 3) (add2 valueToken)))
	 (onBuild
	  ( costAdjust (add 2) (filter ,filter))
	  (buildAdjust (add 2) (filter ,filter))
	  (rentAdjust (add 1) (filter (type #("Commercial" "Residential")) (range 2)))
	  )))))

(define TILE-DECK
  (expandify
  #((0 deck "TileDeck" "tile-deck")	; name of file & object in js/ts
    ;;
    (0 page "Coins" #t)
    (6 fin "ATM"              1 1 1 1 () (coin 1) (subtype "Bank"))
    (5 fin "Bank"             3 2 1 1 () (coin 2) (subtype "Bank"))
    (4 fin "Brokerage"        6 3 2 2 () (coin 3) (subtype "Bank"))
    (3 fin "Stock Exchange"   9 5 2 3 () (coin 5) (subtype "Bank") (vp 1))
    ;; n   type title cost ep stop rent text1 line text2 image

    (0 page "Tiles")
    ;; (12 skip)
    (12 res  "Housing" 2 0 0 "*" "-1 Distance (Owner -2)" (vp "VP" (size 50) (lead 18) (left -8))
	(line 305 BLACK 35 4)
	(text "Cost" 30 328 LEFTJ 35 TEXTFONT BLACK)
	(text "2\n5\n8\n11\n14"		; Costs
	      80 374 RIGHTJ 40 TEXTFONT BLACK)
	(text "House . . . . . .\nTriplex . . . . .\nApartment .\nHigh Rise . .\nTower . . . . . ."
	      110 374 LEFTJ 40 TEXTFONT BLACK)
	(text "VP" 372 328 RIGHTJ 40 TEXTFONT BLACK)
	(text "1\n3\n6\n10\n15"			 ; VP
	      370 374 RIGHTJ 40 TEXTFONT BLACK)	 ; -30 ?
	(text "*Rent" -115 328 LEFTJ 40 TEXTFONT BLACK )
	(text ".5\n1\n2\n4\n8"		; Rent
	      -60 374 CENTER 40 TEXTFONT BLACK)

	;; player.ts: static makeAllPlayers.fixHousingStack()
	;; onDropHousing: incr count as able, subject to limits...
	;; onRemoveHouse: decr count
	(cardProps
	 (rent 0) (vp 0)		; 0 with a vpCounter
	 (onStep (when (isOwner #t) (dist (add -2)))
		 (else (dist (add -1))))
	 ))

    (4 ind "Construction"     1 0 2 1 "+1 Build" (text-low "- $2 on any Build.\nNot less than 1")
       (subtype "Build")
       (cardProps
	(onStep (builds (add 1)))
	(onBuild (buildAdjust (add -2) (min 1) (filter (onCard #t) )))))

    (4 ind "Warehouse"     2 1 1 1 "+1 Build" (text-low "Owner:\n- $2 to Build\nCommercial adjacent.\nNot less than 1")
       (subtype "Build")
       (cardProps
	(onStep (builds (add 1)))
	(onBuild
	 (buildAdjust (add -2) (min 1) (filter (onCard #t) (isOwner #t) (range 1) (type "Commercial"))))))

    ;; TODO code for "dynamic range" based on Owner's ((VP+Props)/10)
    (4 ind "Heavy Equipment"    4 2 2 2 "+1 Build" (text-low "Owner:\n- $2 to Build Transit\nNot less than 1")
       (subtype "Build")
       (cardProps
	(onStep (builds (add 1)))
	(onBuild (buildAdjust (add -2) (min 1)
			      (filter (onCard #t) (isOwner #t) (subtype "Transit"))))))

    ;; industrial, commercial, transit, gov [and anything new]
    (4 ind "Factory"     6 3 2 3 "+1 Build" (text-low "Owner:\n- $2 to Build\nnon-Res/Muni\nNot less than 1")
       (subtype "Build")
       (cardProps
	(onStep (builds (add 1)))
	;; Housing/Home has *type* "Residential", Home has *subtype* "Home"
	(onBuild
	 (buildAdjust (add -2) (min 1)
		      (filter (onCard #t) (isOwner #t) (not (type #("Residential" "Municipal"))))))))
    
    ;; Comercial Buy/Attack
    (4 com "Restaurant" 3 -1 1 1 "+1 Buy" (text-low "-1 Distance (min 1)\nwhen leaving.") (image ())
       (subtype "Bar")
       (cardProps
	(onStep (buys (add 1)))
	(onMove (dist (add -1) (min 1)))))
    (4 com "Bar"        4 -1 2 1 "+1 Buy" (text-low "-1 Distance\nwhen leaving.") (image () center 288 118 187)
       (subtype "Bar")
       (cardProps
	(onStep (buys (add 1)))		; implicit (min 0)
	(onMove (dist (add -1)))))
    (4 com "Night Club" 5 -1 2 2 "+1 Buy" (text-low "-2 Distance\nwhen leaving.")
       (subtype "Bar")
       (cardProps
	(onStep  (buys (add 1)))
	(onMove (dist (add -2)))))	; implict "not less than 0"
    (4 com "Casino"     6 -2 2 2 "+1 Buy" (text-low "Distance = 1\nwhen leaving.") (image () center 320)
       (subtype "Bar")
       (cardProps
	(onStep (buys (add 1)))
	(onMove (dist (set 1)))))
    ;;
    ;; Commercial Attacks

    (4 com "Grocery" 2 0 1 1 "-1 Distance" ; weak attack; low margin business
       (image () fit center nil xs)
       (subtype "Shop")
       (cardProps
	(onStep (dist (add -1)))
	))
    (3 com "Law Office" 3 -1 1 2 "+1 Policy" (text-low "- $3 on Policy actions.")
       (subtype "Shop") (ext "Policy")
       (cardProps
	(onStep (polis (add 1)))
	(onBuild
	 (costAdjust (add -3) (filter (onCard #t) (type "Policy")))) ; triggered by onStop, subject = Player
	))
    (4 com "Cineplex"   4 -1 2 2 () (text-low "-1 Distance\nwhen leaving.") ; ~ "Bar"
       (image () fit top nil xs)
       (subtype "Shop")
       (cardProps
	(onMove (dist (add -1)))))
    (4 com "Dept Store" 5 -1 3 1 "+1 Buy" (text-low "-2 Distance\nwhen leaving.") ; ~ "Night Club"
       (subtype "Shop")
       (cardProps
	(onStep (buys (add 1)))
	(onMove (dist (add -2)))))
    (4 com "Mall"       6 -1 "4*" 2 () (image () fit fit) ; "(temp?) Reverse Direction\nwhen leaving"
       (subtype "Shop")
       (cardProps
	(stop 4)
	(onStop (saveDir reverseDir)) ; this["saveDirMall"] = {"N":"S", "S":"N", "E":"W", "W":"E"}[dir:string]
	(onMove (moveDir (set saveDir))))) ; (whenon ...)? or (on leaving ...); Place a DIR="X" card on dir pile
    (3 com "Stadium"    7 -2 5 2 () (text-low "-2 Distance\nwhen leaving.") (subtype "Municipal") (image ())
       (cardProps
	(onMove (dist (add -2)))))

    ;; Q: Can player activate nextDirection effect to override the binding of Mall?
    ;; originally, no. but maybe that would be a good thing...
    
    ;; MUNI: 
    ;; Maybe Commercial?
    (4 mun "Plaza"      3 0 0 1 "-2 Distance" (image ())
       (text-low "+ $1 Rent, + $1 Wages\nfor adjacent Commercial")
       (cardProps
	(onStep (dist (add -2)))
	(onBuild (rentAdjust (add 1) (filter (range 1) (type "Commercial")))
		 (stopAdjust (add 1) (filter (range 1) (type "Commercial"))))
	))

    ;; PARKS & Recreation
    (4 mun "Playground" 2 -1 0 0 "-1 Distance" (image () fit)
       (text-low "+ $1 Rent adj Residential") (subtype "Park")
       (cardProps
	(onStep (dist (add -1)))
	(onBuild (rentAdjust (add 1) (filter (range 1) (type "Residential"))))
	))
    (4 mun "Park"       4  0 0 1 "-1 Distance" (image () fit) (vp 1)
       (text-low "+ $1 Rent adj Properties") (subtype "Park")
       (cardProps
	(rent 0)			; because is self-adjacent, will be "1"
	(onStep (dist (add -1)))
	(onBuild (rentAdjust (add 1) (filter (range 1))))
	))
    (4 mun "School"     5  1 1 1 "-1 Distance"
       (text-low "+ $1 Rent adj Residential")  (image () fit center nil xs) (vp 1)
       (cardProps
	(onStep (dist (add -1)))
	(onBuild (rentAdjust (add 1) (filter (range 1) (type "Residential"))))
	))
    (4 mun "Lake"       7 () () () "Distance = 1" (subtype "Park") ; not adjacent to other Transit !?
       (text-low "+ $2 Rent\nfor adjacent Properties") (image () fit) (vp 2)
       (text "No Stopping" center 300 CENTER 50 TEXTFONT RED (bold #t))
       (cardProps
	(step 0) (stop 0) (rent 0) (noStop #t) ; cannot be rented, do show show a rentCounter
	(onStep (dist (set 1)))
	(onBuild (rentAdjust (add 2) (filter (range 1))))
	))
    
    ;; GOV
    ;;
    (2 gov "Jail"       1 -1  "0*"  1 "* -1 Buy   \n-1 Build\n-1 Move\n-1 Policy"
       (text-low "-1 Range\nCollect no Rent.") (vp 1)
       (cardProps (stop 0)
		  (onStop
		   (dist (set 0)) (buys (add -1)) (builds (add -1)) (moves (add -1)) (polis (add -1))
		   (noRent (set #t)))
		  (onBuild
		   (rangeAdjustTurn (add -1) (filter (onCard #t)))) ; static effect if occupied
		  (onMove (noRent (set #f))) ; stopOn()/payRent() checks player.noRent 
		  ;; pro'ly the other player should pay Rent to the City (to support your stay in Jail)
		  ))
    (4 gov "County Recorder" 3 0  0  1 "+1 Buy\n+1 Build\n+1 Range" (text-low "- $3 on Build") (vp 1)
       (cardProps
	(onStep (buys (add 1)) (builds (add 1)) (rangeAdjustTurn (add 1)))
	(onBuild
	 (buildAdjust (add -3) (filter (onCard #t))))
	)) 
    (4 gov "Enterprise Zone" 4 0  2  1 "+1 Buy\n+1 Build"
       (text-low "- $1 to Build\nadjacent Property.\n+ $1 Rent on adjacent\nnon-Residential Property.") (vp 1)
       (cardProps
	(rent 0)			; will self-adjust to 1
	(onStep (buys (add 1)) (builds (add 1)))
	(onBuild (buildAdjust (add -1) (filter (onCard #t)))
		 (rentAdjust (add 1) (filter (range 1) (not (type "Residential")))))
	))

    (1 gov "Court House"     5 0  0  1 "+1 Buy\n+1 Build" (text-low "+1 Policy\n- $3 on Policy actions") (vp 2)
       (ext "Policy")
       (cardProps ;; buys, draws, buys, builds, polis reset each turn.
	(onStep (buys (add 1)) (builds (add 1)))
	;; onCard continues beyond the one-shot check by onStop
	(onBuild
	 (polisAdjust (add 1) (filter (onCard #t)))
	 (costAdjust (add -3) (filter (onCard #t) (type "Policy"))))
	))
    ;; *-text means: onStop [implemented as onBuild (onCard #t)
    ;; text-low impicitly means: "while player is on this tile"  (onCard #t)
    (1 gov "City Hall"       6 0 "*" 1 "* +1 Buy  \n+1 Range" (text-low "+1 Policy\n- $3 on Policy actions") (vp 2)
       (ext "Policy")
       (cardProps
	(stop 0)
	(onStep (buys (add 1)) (rangeAdjustTurn (add 1)))
	(onBuild
	 (polisAdjust (add 1) (filter (onCard #t)))
	 (costAdjust (add -3) (filter (onCard #t) (type "Policy"))))
	))

    ;; "Policy Action" is: Drop(on plyrPolis,tablePolis)/Build/Enact or Revoke/Overlay/Discard

    ;; if gov building falls off the end of auction, next player *must* Place it at start of turn.
    ;; ... so if you push it off, other player must to Place it for no cost; (but consumes a Build action? no...)
    ;; other player does not *own* it; must pay Cost+Rent to acquire
    ;; (0 page "TileBack" #t) 		; page-eject, make back on clean sheet:
    (mini-vec0 0 BACK-DECK "Tile Back")
    )))


(define TRANSIT-DECK 
  (expandify
  #((0 deck "TransitDeck" "transit-deck") ; name of file & object in js/ts
    ;;
    (0 page "Transit")
    
    ;; TRANSIT -- implicit (onStop (dist (set 1)))
    ;; transitTo: (transitTo (filter ...dest)) (dist (set 1)) ; override any distChoice/distAdjust
    ;; Airport & Train Station ignore Roads (& RoadBlocks)
    ;; Bus Stop follows Roads, and will not stop on them (unless they loop)
    ;; Taxi & Transit Hub move normally, following  Roads
    ;; Train and TransitHub (&Airport) accept transfer from BusStop! 
    ;; you *can* take Bus or Taxi to "Train Station" or "Airport" and transitTo next station/airport
    ;; Bus stops at (endsWith Transit);
    ;; placement constrained by (beginsWith Transit) : Lake, but NOT Hub/Taxi

    (4 mun "Bus Stop"    4 0 "1*" 1 "-1 Distance\n\n*Move to next Transit\nor Com-Transit.\nIf that is Bus Stop:\nDistance = 1"
       (subtype "Transit")		; transitTo: if loop, just (step 1) in moveDir
       (ext "Transit")
       (cardProps	  ; net cost (stop-rent) = $0
	(stop 1)	  ; go to a Transit/Com-Transit, try stop there (dist=0); if Bus, step 1
	(onStep (when (fromTransit "Bus Stop") (dist (set 1))) ; normal transit, override (onTransit (dist 0))
		(else (dist (add -1))))			       ; 
	(onStop
	 (transitTo (subtype #("Transit" "Com-Transit")) (dist 0) (roads #t))) ; stop before you go
	))
    (4 mun "Train Station" 5 -1 "1*" 1
       "*Go to next\nTrain Station\nin chosen Direction.\nDistance = 1"
       (subtype "Transit") ; at first station, (- $1 step, get $1 stop) pay $1 rent, next station step(- $1)
       (ext "Transit")
       (cardProps		     ; net= -$2: depart=(-1, +1 -1),  arrive=(-1) Owner: (2 + 1)
	(stop 1)
	(onStop
	 (when (chooseDir ((N 0) (S 0) (E 0) (W 0))) ; set player.moveDir; $0 {C:undefined}
	       (transitTo (name "Train Station")))) ; (dist (set 1)) (fromTransit "Train Station")
	))
    ;; Transit Hub: may buy another Move
    (4 com "Transit Hub" 6 -1 "1*" 1 "-1 Distance\n*May pay $3 to Move\nnext Distance\nin chosen Direction." (vp 1)
       (text-low "- $2 to Build\nadjacent Transit") ; pay, pick dir, discover distance
       (subtype "Com-Transit")
       (ext "Transit")
       (cardProps
	(stop 1)
	(onStep (dist (add -1)))
	(onStop
	 (when (chooseDir 3) ; player can choose ("same dir" for $0) or ("N","E","W","S" for $4)
	       (payOwner 3) (temp (set moveNextDistance)))) ; move from onStop, newMove
	;; Taxi & Bus Stop here, but we don't encourage them to START here.
	(onBuild (buildAdjust (add -2) (filter (range 1) (onCard #t) (subtype #("Transit")))))
	))

    ;; Taxi extend the current Move
    ;; Taxi is expensive to avoid intentional looping.
    ;; Consider to have owner pay "tax" when riding own Taxi: taxPay instead of rentPay
    ;; !! Taxi does not need to be (subtype: transit) because the (dist: 1) restriction does not apply!
    ;; !! should be OK to put Taxi adjacent to Airport or Train Station; [like Transit Hub]
    ;; !! but not legal to stack multiple Taxis adjacent! TODO
    (4 com "Taxi"	 6 -1 () () "Use Next Distance\nPay $1 per Distance\nand continue to Move."
       (subtype "Com-Transit") (image ())
       (ext "Transit")
       (cardProps
	(stop 0) (rent 0) (noStop #t)	; never stops here!
	(onStep
	 ;; Not a "Transit"; just extend the Distance and keep moving.
	 ;; setting 'fromTransit' would mean we Taxi *through* the TrainStation/Taxi/Airport!
	 (dist (set nextDistance))	; flip Next Distance
	 (payOwner (set dist))		; pay for total distance, not just the increment! (?)
	 )
	))
    (4 mun "Airport"	 8 0 () "1*"	; "Continue in Direction\nto next Airport"
       ("Go to next Airport.\nDistance = 1\n\n*To each Airport\npay $1 + $1 per range\nto destination."
	(size 45)) (image ()) (vp "*")
       (text-low "*VP = {5, 3, 1, 0}" (size 45))
       (subtype "Transit")
       (ext "Transit")
       (cardProps
	(cardFields "nthAirportVP")	     ; nthAirportVP is ALSO a NamedValue!!
	(nthAirportVP (set #(0 5 3 1 0 0)))  ; indexed by {1..4} for 4 cards
	(stop 0) (rent 1) (noStop #t)	     ;
	(vp (set nthAirportVP))		; create vpCounter: nthAirportVP is ALSO a NamedValue 
	(onStep
	 (when (ne (arrivalFrom  "Airport")) ; (cardFields "arrivalFrom") is now: Player.arrivalFrom
	       (arrivalFrom (set "Airport"))
	       (temp (set rangeToAirport))
	       (payOwner (set temp) (add rentAdjust)) ; pay Rent at departure
	       (transitTo (name "Airport")))	      ; (dist 1) (fromTransit "Airport")
	 (else					      ; arrival from other "Airport"
	  (payOwner (set temp) (add rentAdjust))      ; pay Rent at arrival
	  (arrivalFrom (set undefined)))	      ; get off the plane...
	 )
	))
    )))

(define REST
  #(
    ))
;;

;; because Jail may not exist, we impose some punishment:
(define (goJail nCards title descr xname key_player)
  (let* ((text (string-append descr "Go to Jail."))
	 (lowtext "*If no Jail:\nbuys = builds = policies = 0"))
    `(,nCards event ,title RED ,text () ,lowtext
	 (subtype "Gov") (xname ,xname)
	 (step 2)
	 (cardProps
	  (event (withPlayer ,key_player
			     (when (goTo (name "Jail")))
			     (else (buys (set 0)) (builds (set 0)) (polis (set 0)))))))

    ))

(define EVENT-DECK
  (expandify
  #((0 deck "EventDeck" "event-deck")         ; Class name and Filename.ts
    ;; nreps type COLOR "title" "text" cost "subtext"
    ;; YELLOW - Policy; Cost: Boon, No-Cost: Hex (Table)
    ;; ORANGE - Temp Policy;        No-Cost: Hex (Table)
    ;; BLUE - Event future/deferred
    ;; RED - bad Event
    ;; GREEN - happy Event (BLUE?)
    (0 page "Event")
    ;; n type title text [cost] [below-line-text]

    ;; (2 event "Jackpot" GREEN "+$ 20" () () ; enable buy/build High-Tech!
    ;;    (cardProps
    ;;  (event (coins (add 20)))))
    ;; (2 event "IPO" GREEN "+$ 20" () ()  ; enable buy/build High-Tech!
    ;;    (cardProps
    ;;  (event (coins (add 20)))))

    ;; 'future': you buy it now, activate it *once* in the future [by discarding]
    ;; (when (offerChoice "") (action...))
    ;; offer to activate during 'whenCond'

    ;; offerChoice: hightlight card, if D&D->Discard activate it; if click Dist card/stack, continue;
    ;; onGetDist: after flipping distCard, reading player.dist

    ;; chooseDist: highlight the Policy/Event (may be in Discard),
    ;; initial/default: chooseDist = { 0 } (no real choice...) is a bag, not a set; uniquify on use.
    ;; put distCounter(s) Center/Large of "distance" card/stack,
    ;; put (+) (OK) (-) buttons at Bottom of "distance" card/stack
    ;; after (OK) retry: onGetDist to offerChoice [another] futureEvent..


    ;; NOTE: to setup onGetDist-->offerChoice, DR must be activated during payCost;
    ;; do not wait for onDiscard/isDiscardActivated:
    ;; OR-- we could search policySlots/plyrProjs for applicable Policy/Event... [nah]
    (2 future "Increase Distance" BLUE "+1 Distance." 2 ()
       (ext "Event2")
       (step 3)
       (cardProps
        (onGetDist (when (offerChoice "+1 Distance?") (dist (add 1))))))
    
    (2 future "Adjust Distance 1" BLUE "+1 Distance\nor\n-1 Distance" 3 ()
       (ext "Event2")
       (step 3)
       (cardProps
        (onGetDist (when (offerChoice "Adjust Distance?") (chooseDist (high 1) (low -1) )))
        ))

    (2 future "Adjust Distance 2" BLUE "+2 Distance\nor\n-2 Distance" 4 ()
       (ext "Event2")
       (step 3)
       (cardProps                       ; (onGetDist (distChoice (include [...])))
        (onGetDist (when (offerChoice "Adjust Distance?") (chooseDist (high 2) (low -2) )))
        ))

    (2 future "Next Distance" BLUE "Use next Distance." 3 () 
       (ext "Event2")
       (step 3)
       (cardProps
        (onGetDist (when (offerChoice "Adjust Distance?") (dist (set nextDistance))))
        ;; see also: mayRejectDist; consider: (dist (add nextDistance)) == guaranteed value!
        )) 
    
    ;; NOTE: may want to have (subtype "Tax") vs ("Gov")
    (2 future "A Tax Lawyer" BLUE "Pay only half of Tax assessment.\n(round up)" 3 ()
       (ext "Event2")
       (step 3)
       (cardProps
        ;; Modal screen must set Active Player!?, multiple players may have sequential offers:
        ;;(onTax (when (offerChoice "Reduce Taxes?") (taxRateAdjust (set .5))))
        ;; If we have "undoLastEffect", then Player can discardActivate on next turn (and avoid continual offers...)
        (futureEvent (undoLastEffect (subtype Tax))) ;
        ))
    (2 future "A Good Lawyer" BLUE "Avoid the effect a Policy\n or Gov Event" 4 ()
       (ext "Event2")
       (step 3)
       (cardProps                   ; 
        ;;(onEffect (when (offerChoice "Use Lawyer?") (undoLastEffect (or (type Policy) (subtype Gov)))))
        (futureEvent (undoLastEffect (or (type "Policy") (subtype Gov))))
        ))

    ;; below, (event x y z) ==> ( x y z (discard this)) [when flip a Event card]
    ;; macro (futureEvent x y z) ==> (on (discard (x y z)))   [when discard a Future Event]

    ;; compare to:
    ;; "Next Draw costs 2 Draws" (on (draw (draws (add -1))))
    ;; "Next Draw is Free Draw" (on (draw (draws (add 1))))
    (2 future "Draw Another" BLUE "Draw another card\nwhen you Draw." 4
       "Process drawn cards in any order."
       (ext "Event2")
       (step 3)
       (cardProps
        ;; "draw" action reads drawNAdjust then removes from the bag
        (futureEvent (untilDraws (drawNAdjust (add 1)))) ; Player(s)
        ))
    (2 future "Change Direction" BLUE  "Select Next Direction" 2 ()
       (ext "Event2")
       (step 3)
       (cardProps
        (futureEvent (direction (set nextDirection)))))
    (3 future "Choose Direction" BLUE  "Set direction until Next Direction" 3 ()
       (ext "Event2")
       (step 3)
       (cardProps
        (futureEvent (chooseDir ((N 0) (S 0) (E 0) (W 0))) (direction (set moveDir)))))
    (2 future "Draw Again" BLUE "+1 Draw" 2 ()
       (ext "Event2")
       (step 3)
       (cardProps
        (futureEvent (draws (add 1)))))
    (2 future "Move Again" BLUE "Move by Next Distance" 4 ()
       (ext "Event2")
       (step 3)
       (cardProps
        (futureEvent (temp (set moveNextDistance)) )))
    (2 future "New Move" BLUE "Choose Direction\nMove by Next Distance" 5 ()
       (ext "Event2")
       (step 3)
       (cardProps
        (futureEvent (when (chooseDir ((N 0) (S 0) (E 0) (W 0))) (temp (set moveNextDistance))))))
    ;;
    (2 future "Increased Range" BLUE "+1 Range of Action" 2 () ; untilTurnEnd
       (ext "Event2")
       (step 3)
       (cardProps
        (futureEvent (rangeAdjustTurn (add 1)))))
    (2 future "Bonanza" BLUE "+1 Buy\n+1 Build\n+1 Range" 6 ()
       (ext "Event2")
       (step 3)
       (cardProps
        (futureEvent (buys (add 1)) (builds (add 1)) (rangeAdjustTurn (add 1)))))
    (2 future "Real Estate Deal" BLUE "When a Player buys a Property\nyou may buy it from them." 4
       "+1 Buy to Other Player."
       (ext "Event2")
       (step 3)
       (cardProps 
        (onBuy (record PlayerLastBuy)) ; pro'ly do this always; for easy UNDO
        ;; [vs finding a way to remove this effect:  (removeBuyHandler)]
        (futureEvent (buy PlayerLastBuy)))) ; (coins_add cost/-cost) (buys_add a) (whenyesbuycard "do you want?")
    (2 future "Urban Renewal" BLUE "Pay $2 : Discard an eligible* tile\n+1 Buy\n+1 Build" 3
       "*not owned by other Player & VP = 0"
       (ext "Event2")
       (step 3)
       (cardProps
        (futureEvent (builds (add 1)) (buys (add 1)) (doUrbanRenewal 2)) ; legalMark/buildCost on clickable Tiles

        ;; Q: must player build on a vacant Slot?
        ;; A: no. 
        ;; Q: Does Player need the extra $2 to trigger this? 
        ;; A: configBuildCost will include the $2 on the vacant lots!
        ;;
        ;; we should addCounter to the Card while floating on the Drag layer.
        ;; value = configBuildCost (with buildAdjust, range effects, and UrbanRenewal if over a vacant lot)
        ))

    ;; first Event with no cost! [auto-move to plyrPrjs (holding area); Discard to trigger effect]
    ;;
    ;; (untilDraws XX) ==> (discardTilDraw XX) ==> (on discard XX) (on draw (undo XX))
    ;;
    ;; Draw *always* discards all of this Player's deferred Events. (with no effects)
    ;; so: do NOT discard; If you want to avoid the effect: just Draw and it will expire.
    ;;
    ;; the Card is good until next draw, the effect may last beyond.
    ;; so generally, we can just activate on next draw...
    ;; as with future: activate on discard: but NextDraw -> Discard.

    (2 deferred "Demolition" BLUE
       "Pay $2 to discard an eligible* tile." ()
       "Discard on your next Draw.\n*not owned by other Player & VP = 0" ; Bring-Your-Own-Build; this card just brings the bulldozer
       (ext "Event")
       (step 3)
       (cardProps
        (event (doUrbanRenewal 2))))    ; set legalMark/buildCost on clickable Tiles
    (2 deferred "Another Move" BLUE "+1 Move\nWith next Distance" () "Discard on your next Draw."
       (ext "Event")
       (step 3)
       (cardProps
        (moves (add 1)) ))              ; moves is reset by TurnBegin
    (2 deferred "Another Buy" BLUE "+1 Buy" () "Discard on your next Draw."
       (ext "Event")
       (step 2)
       (cardProps
        (buys (add 1))))                ; buys is reset by TurnBegin.
    (2 deferred "Another Build" BLUE "+1 Build" () "Discard on your next Draw."
       (ext "Event")
       (step 2)
       (cardProps
        (builds (add 1))))              ; builds is reset  by TurnBegin
    (2 deferred "Buy Discount" BLUE "- $3 on your next Buy\nNot less than 0" () "Discard on your next Draw."
       (ext "Event")
       (step 3)
       (cardProps
        (untilBuys (costAdjust (add -3) (min 0))) ; until Player buys, card.costAdjust -= 3
        ))
    (2 deferred "Discount Buy" BLUE "Buy cost reduced by range\non your next Buy\nNot less than 1" () "Discard on your next Draw."
       (ext "Event")
       (step 1)
       (cardProps
        (untilBuys (costAdjust (sub range) (min 1))))) ; until Player buys, player.buildAdjust = -99

    (2 deferred "Build Discount" BLUE "- $3 on your next Build\nNot less than 0" () "Discard on your next Draw."
       (ext "Event")
       (step 3)
       (cardProps
         (untilBuilds (buildAdjust (add -3) (min 0))) ; until Player builds, card.buildAdjust -= 3
        ))
    (2 deferred "Discount Build" BLUE "Build cost reduced by range\non your next Build\nNot less than 1" () "Discard on your next Draw."
       (ext "Event")
       (step 1)
       (cardProps
        (untilBuilds (buildAdjust (sub range) (min 1))))) ; until Player builds, card.buildAdjust -= player.adjustedRange

    (2 deferred "Enterprise" BLUE "+1 Buy\n+1 Build\n+1 Range" () "Discard on your next Draw."
       (ext "Event")
       (step 3)
       (cardProps
        (buys (add 1)) (builds (add 1))
        (untilTurnEnd (rangeAdjustTurn (add 1)))))
    ;;
    ;; (event...) auto-discards this card

    ;; GREEN Events: Bonus
    (1 event "Time Off" GREEN "Player with highest Cash:\nGo to Home.\n(and stop there)" () ()
       (subtype "Bonus")
       (step 1)
       (xname "-cash")
       (cardProps
        (event (withPlayer high_total_cash (goTo (subtype "Home") (isOwner #f))))))
    (1 event "Time Off" GREEN "Player with highest Property:\nGo to Home.\n(and stop there)" () ()
       (subtype "Bonus")
       (step 1)
       (xname "-property")
       (cardProps
        (event (withPlayer high_total_cost (goTo (subtype "Home") (isOwner #f))))))
    (1 event "Time Off" GREEN "Player with highest Rent:\nGo to Home.\n(and stop there)" () ()
       (subtype "Bonus")
       (step 1)
       (xname "-rent")
       (cardProps 
        (event (withPlayer high_total_rent (goTo (subtype "Home") (isOwner #f))))))
    (2 event "Win Lottery" GREEN "+ $2 x Next Distance" () () 
       (subtype "Bonus")
       (step 3)
       (cardProps
        (event  (coins (add2 nextDistance)))))
    (2 event "Go Home" GREEN "Go to your Home.\n(and stop there)" () ()
       (subtype "Bonus")
       (step 1)
       (cardProps
        (event (goTo (subtype "Home") (isOwner #f)) (dist (set 0))))) ; #f: test owner of dest, not the source
    ;; with January semantics, this should .stepon(home) .stopon(home) so you get $1

    ;;
    ;; RED events [Gov, Damage] in the Property Deck:
    ;;
    (2 event "Income Tax" RED "All players pay 20%\n(round up)\nof the total Rent\nfor Property they own."
       () () (subtype "Tax")
       (step 2)
       (cardProps (rent .2)
        (event (withPlayer #t) (pay_tax total_rent) )))
    (2 event "Property Tax" RED "All players pay 20%\n(round up)\nof the total Cost\nfor Property they own."
       () () (subtype "Tax")
       (step 2)
       (cardProps (rent .2)
        (event (withPlayer #t) (pay_tax total_cost))))
    (2 event "Wealth Tax" RED "All players pay 20%\n(round up)\nof their Cash on hand." ()
       "Do not count debt or Projects." (subtype "Tax")
       (step 2)
       (cardProps (rent .2)
        (event (withPlayer #t) (pay_tax total_cash))))
    
    (goJail 2 "Busted" "" "" 'this_player)
    (goJail 1 "Accounting Fraud" "Player with most Cash on Hand\n" "-cash" 'high_total_cash)
    (goJail 1 "Investment Fraud" "Player with highest Property Cost\n" "-property" 'high_total_cost)
    (goJail 1 "Price Gouging" "Player with highest Rent\n" "-rent" 'high_total_rent)
    (goJail 1 "Contract Fraud" "Player with most Roads\n" "-roads" 'high_total_roads)
    ;; "Privatization" "Hostile Takeover" "Condemnation"
    (2 event "Eminent Domain" RED "City pays owner the Cost\nof current tile (if VP = 0)\nand removes owner token."
       () "May buy this tile for Cost + Rent + $2." (subtype "Gov")
       (step 3)
       (cardProps
        (event (withPlayer #t)
	       (when (eq (vp 0))                           ; 
                     (withPlayer owner (coins (add cost))) ; interesting when vcOwned! (adjPlyrCoins?)
                     (owner (set "undefined"))             ; creates a *vacant* property
		     (offerBuyTile ())
                     ))))

    (2 event "Failed Inspection" RED "Put a NoRent token on this property"
       () "remove when Owner stops there\n* except non-Stop: Roads, Airport, etc" (subtype "Gov")
       (step 2)
       (cardProps
        (event (event (withPlayer #t) (damage (noRent #t) (filter (range 0) (from curPlayerCard)))))))

    ;; generic (damage...) remove 2 houses or put 2 Debt-per-Rent
    (1 event "Earthquake" RED "On each Housing development:\nPay $2 per $* of Rent\nor downgrade largest building." ()
       "Next Alignment"
       (subtype "Damage")
       (step 3)
       (cardProps
        (event (withPlayer #t) (damage (downgrade 2) (filter (name "Housing"))) ; reduces VP!
               (nextAlignment #t))))

    (1 event "Major Flooding" RED "Put a NoRent token on \nproperty* within range = 1" ()
       "remove when Owner stops there\n* except non-Stop: Roads, Airport, etc"
       (subtype "Damage")
       (step 2)
       (cardProps
        (event (withPlayer #t) (damage (noRent #t) (filter (range 1) (from curPlayerCard))))))
    ;; (noStop == true) 
    
    (1 event "Fire Damage" RED "Owner pays $2 for each Property\nwithin range = 1 of Player" () ()
       (subtype "Damage")
       (step 3)
       (cardProps
        (event (withPlayer #t) (damage (coins -2) (filter (range 1) (from curPlayerCard) (not (type "Road")))))))

    (1 event "Biohazard Cleanup" RED "On each Housing development:\nPay $2 per $* of Rent\nor downgrade largest building." () ()
       (subtype "Damage")
       (step 3)
       (cardProps
        ;; declare 'house' as a Cardfield? set house(n) { do the needful... }
        (event (withPlayer #t) (damage (downgrade 2) (filter (name "Housing")))))) 

    (1 event "Toxic Spill" RED ("Each Player must demolish a property\n within range = 2 of current Player,\nand receives $2 if they do." (size 42)) ()
       "Tiles with VP and Roads are exempt"
       (subtype "Damage")
       (step 3)
       (cardProps
        (event (withPlayer #t) (toxicSpill (range 2) (comp 2)))
        ))
    (1 event "Tornado" RED "Demolish 3 properties\nwith lowest Cost + Rent\nwithin range = 3 of current Player" ()
       "Tiles with VP and Roads are exempt"
       (subtype "Damage")
       (step 3)
       (cardProps
        (event (withPlayer #t) (tornado (range 3) (count 3)))
        ))
    ;;
    ;; (0 page "EventBack" #t)             ; page-eject, make back on clean sheet:
    (mini-vec0 0 BACK-DECK "Event Back")
    )))

(define JAIL-DECK 
  #((0 deck "JailDeck" "jail-deck")
    (goJail 2 "Busted" "" "" 'this_player)
    (goJail 1 "Accounting Fraud" "Player with most Cash on Hand\n" "-cash" 'high_total_cash)
    (goJail 1 "Investment Fraud" "Player with highest Property Cost\n" "-property" 'high_total_cost)
    (goJail 1 "Price Gouging" "Player with highest Rent\n" "-rent" 'high_total_rent)
    (goJail 1 "Construction Fraud" "Player with most Roads\n" "-roads" 'high_total_roads)
    ))
(expandify JAIL-DECK)

(define (temp-policy name text step opers)
  (let ((ext (if (equal? name "Road Repair") "Dir" "Policy")))
  `(2 temp ,name ORANGE ,text ()	; [cost text2 ...]  == ()
      "Place 3 owner tokens on this card. At start of your turn\nremove one. When all are gone discard this card."
      (ext ,ext)
      (step ,step)
      (cardProps
       ,@opers
       ;; will be acting on *own* card fields (card0[fieldName]) !
       (cardFields #("turnToken" "turnTokenCounter")) ; card0.turnToken
       (turnToken 3)
       (turnTokenCounter (counter #("turns left"))) ; player.color
       (onTurnStart (turnToken (add -1) (filter (isOwner #t)))
		    (when (le (turnToken 0)) (discard #t)))
       ))))

(define POLICY-DECK
  (expandify

  ;; Q: does one pay to establish a Policy?
  ;; Yes: Cost includes "policy action" to put it effect.
  ;;      Therefore; you must have a Policy Action to Buy a Policy!
  ;; No: you can hold a Policy until you have a PA and $ to put it in effect.
  ;;     only 1 (or 2) Policy holding slots on your board
  ;;     can Buy a Policy and discard it (or one in your slot)
  ;; same cost is paid to remove an active Policy (public or attack or boon)
  ;;     or/only/especially at beginning of game...?
  ;;
  ;; Q: "Boom Times" is a powerful boon. should it be marked as "public only" ?
  ;; IMPLICIT: card.isPolicy -> (onDiscard (removeRegistry {subj:card, *}))
  ;; (onBuy/Activate XX) => (addToRegistry {subj:card, *})


  #((0 deck "PolicyDeck" "policy-deck")       ; name of object in js/ts
    (0 page "Policy")

    ;;     (0 page "PolicyBack" #t)                     ; page-eject, make back on clean sheet:
    ;;     (18 back "Policy Back" GREY #f 260 260 "Policy")
    ;;     ))
    ;; (expandify POLICY-DECK)

    ;; (define POLICY-DECK0
    ;;   #((0 deck "PolicyDeck" "policy-deck")        ; name of object in js/ts
    ;;     (0 page "Policy")
    ;; n type title text [cost] [below-line-text]
    (2 policy "Increase Distance" YELLOW   "+1 Distance on each Move" 3 ()
       (step 2)
       (cardProps
        (onMove (dist (add 1))))) ; Note: may happen before OR after distChoice...

    (2 policy "Adjust Distance 1" YELLOW "+1 Distance\nor\n-1 Distance" 4 "Not less than 1."
       (step 3)
       (cardProps
        (onBuild (distAdjust (distChoice (high 1) (low -1))))))
    (2 policy "Adjust Distance 2" YELLOW "+2 Distance\nor\n-2 Distance" 5 "Not less than 1."
       (step 3)
       (cardProps
        (onBuild (distAdjust (distChoice (high 2) (low -1))))))
    ;;
    (2 policy "Buy More" YELLOW "+1 Buy each turn." 5 ()
       (step 3)
       (cardProps
        (buys (add 1))
        (onTurnStart (buys (add 1))))) ; More Buy (buysAdjust ?)

    (2 policy "Build More" YELLOW "+1 Build each turn." 4 ()
       (step 3)
       (cardProps
        (builds (add 1))
        (onTurnStart (builds (add 1))))) ; (buildsAdjust ?)

    (2 policy "Build Farther" YELLOW "+1 Range of Action" () ()
       (step 2)
       (cardProps
        (onBuild (rangeAdjust (add 1))))) ;; Gov decree

    (2 policy "Build Nearer" YELLOW  "-1 Range of Action" () "Not less than 1."
       (step 2)
       (cardProps
        (onBuild (rangeAdjust (add -1) (min 1)))))
    (2 policy "Fuel Rationing" YELLOW "-1 Distance on each Move" () "Not less than 1."
       (step 2)
       (cardProps
        (onMove (dist (add -1) (min 1))))) ; or (distAdjust ?)
    ;; when computing effective distance or range: include distAdust & rangeAdjust
    ;; if table.polis then apply to all players
    ;;(2 policy YELLOW "Family Values" "+ $2 for stopping at your Home." 3)

    (2 policy "Draw Another" YELLOW "Pay $1 and Draw another card\nwhen you Draw." 4 "Process drawn cards in any order."
       (step 3)
       (cardProps
        (onBuild (coins (add -1)) (drawNAdjust (add 1))))) ; when doing "draw", draw N cards.
    (2 policy "Draw Again" YELLOW "Pay $1 and get +1 Draw\nat start of each turn." 4 "whether or not you use the Draw"
       (step 3)
       (cardProps
        (onTurnStart (coins (add -1)) (draws (add 1))))) ; at start of turn, set player.draws = turnDraws
    (2 policy "Move Again" YELLOW "Pay $1 and get +1 Move Action\nat start each turn." 4 ()
       (step 2)
       (cardProps
        (onTurnStart (coins (add -1)) (moves (add 1)))))
    (2 policy "Flexible Itenerary" YELLOW "When you flip a first Distance card\nyou may flip a second Distance card." 3 ()
       (step 2)
       (cardProps
        (onGetDist (when (offerChoice "Flip second dist?") (dist (set nextDistance)))))) ;; 
    ;; propbably generic code to activate a Policy.. if card.cont == table.policySlots vs player.plyrPolis
    ;;
    (2 policy "Road Repair" YELLOW "Movement N is blocked." () () (xname "-N")
       (ext "Dir")
       (step 2)
       (cardProps
        (onBuild (blockedDirAdjust (include #( "N" )))))) ; blockedDir is a Bag, not a set
    (2 policy "Road Repair" YELLOW "Movement E is blocked." () () (xname "-E")
       (ext "Dir")
       (step 2)
       (cardProps
        (onBuild (blockedDirAdjust (include #( "E" )))))) ; maybe: (blockedDir (include "E"))
    (2 policy "Road Repair" YELLOW "Movement S is blocked." () () (xname "-S")
       (ext "Dir")
       (step 2)
       (cardProps
        (onBuild (blockedDirAdjust (include #( "S" ))))))
    (2 policy "Road Repair" YELLOW "Movement W is blocked." () () (xname "-W")
       (ext "Dir")
       (step 2)
       (cardProps
        (onBuild (blockedDirAdjust (include #( "W" ))))))
    (2 policy "Discount to Build" YELLOW "- $1 on each Build" () "Not less than 1."
       (step 1)
       (cardProps
        (onBuild (buildAdjust (add -1) (min 1)))))
    (2 policy "Bail Bond" YELLOW "No effects when you Stop in Jail" () ()
       (step 1)
       (cardProps                       ; OR: all Cards are suppressible by name. doResponses checks.
        (special (suppressCard "Jail"))))

    ;; move dist *then* apply penalties:
    (2 policy "Overtime Penalty" YELLOW "-1 Move\n- $2\nIf you move Distance 5 or more." () ()
       (step 1)
       (cardProps                       ; move may be blocked at less than orig distance, or extended by transit
        (onStop (when (gt (distMoved 4)) (coins (add -2)) (moves (add -1))))))
    (2 policy "Overtime Bonus" YELLOW "+ $2\nIf you move Distance 5 or more." () ()
       (step 1)
       (cardProps                       ; move may be blocked at less than orig distance, or extended by transit
        (onStop (when (gt (distMoved 4)) (coins (add 2))))))

    (2 policy "Speed Limit" YELLOW "Maximum Distance = 4." () ()
       (step 1)
       (cardProps
        (onMove (dist (max 4)))))
    ;;(2 policy YELLOW "Shoddy Workmanship" "Remove 1 House if you stop\nat your own Residential property." 4 ())
    ;; OR remove on BUILDING at your Residential Property (so: if you have ONLY a Tower...)

    (2 policy "Minimum Wage" YELLOW " $2 min Wages" 2 ()  ; or 0? 
       (step 2)
       (cardProps
        (onBuild (stopAdjust (min 2) )))) ; note that later effects may override
    (2 policy "Labor Shortage" YELLOW "+ $2 to all Wages" 3 ()
       (step 2)
       (cardProps
        (onBuild (stopAdjust (add 2))))) ; for each card on which player steps this turn
    ;;
    (2 policy "Urban Renewal" YELLOW "Pay $2 to build on eligible* lot." 4
       "*not owned by other player & VP = 0"
       ;; dragStart: identify vacant lots, dropped: remove tile (return policy to policySlots)
       (step 3)
       (cardProps
        (special (doUrbanRenewal 2))))  ; legalMark/buildCost on clickable Tiles
    ;; dragStart: identify vacant lots, dropped: remove tile (return policy to policySlots)
    ;; (buildAdjust (add 2)), allowDrop() 

    (2 policy "Zoning: No Houses" YELLOW "May not build Houses.\n(on Residential)" () ()
       (step 2)
       (cardProps
        (special (configBuy NoHouse)))) ;
    (2 policy "Zoning: Only Houses" YELLOW "May only build Houses.\n(on Residential)" () ()
       (step 2)
       (cardProps
        (special (configBuy OnlyHouse)))) ; (on (configBuy ...))

    (1 policy "Price of Power" YELLOW "- $1 at start of turn\nfrom Player with highest\nCash on hand."
       3 () (xname "-cash")             ; filter on target player == curPlayer ?? AND/OR need way to sell props to city
       (step 1)
       (cardProps
        (onTurnStart (withPlayer high_total_cash (coins (add -1))))))
    (1 policy "Price of Power" YELLOW "- $2 at start of turn\nfrom Player with highest\nProperty Cost."
       4 () (xname "-property")
       (step 2)
       (cardProps
        (onTurnStart (withPlayer high_total_cost (coins (add -2))))))
    (1 policy "Price of Power" YELLOW "- $3 at start of turn\nfrom Player with highest\ntotal Rent."
       5 () (xname "-rent")
       (step 3)
       (cardProps                       ; each turn, compute (player high_total_rent) act if == curPlayer
        (onTurnStart (withPlayer high_total_rent (coins (add -3))))))

    (temp-policy "Boom Times" "+1 Buy\n- $2 to Build.\nNot less than 1."
                 1 '((buys (add 1)) (onBuild (buildAdjust (add -2) (min 1)))))
    (temp-policy "Fuel Shortage" "-1 Distance.\nNot less than 1."
                 2 '((onMove (dist (add -1) (min 1)))))
    (temp-policy "Road Repair" "EW movement is blocked"
                 2 '((onMove (blockedDirAdjust (include #( "E" "W" ))))))
    (temp-policy "Road Repair" "NS movement is blocked"
                 2 '((onMove (blockedDirAdjust (include #( "N" "S" ))))))

    ;; Use "Event Back"
    )))

(define TEMP-DECK
  (expandify
   #(
     (temp-policy "Boom Times" "+1 Buy\n- $2 to Build.\nNot less than 1."
		  1 '((buys (add 1)) (onBuild (buildAdjust (add -2) (min 1)))))
     (temp-policy "Fuel Shortage" "-1 Distance.\nNot less than 1."
		  2 '((onMove (dist (add -1) (min 1)))))
     (temp-policy "Road Repair" "EW movement is blocked"
		  2 '((onMove (blockedDirAdjust (include #( "E" "W" ))))))
     (temp-policy "Road Repair" "NS movement is blocked"
		  2 '((onMove (blockedDirAdjust (include #( "N" "S" ))))))
    
     )))

;; (define (color-dots color dist)
;;   let ((num (if (< dist 4) 2 1))
;;        (cs (symbol->string color)))
;;   `(,num mov ,(string-append cs "-" dist) ,color ,dist)
;;   )
;; (color-dots PURPLE 1)
;; (color-dots PURPLE 2)
;; (color-dots PURPLE 3)
;; (color-dots PURPLE 4)
;; (color-dots PURPLE 5)
;; (color-dots PURPLE 6)


(define DOTS-DECK
  #((0 deck "DotsDeck" "dots-deck")	      ; name of object in js/ts
    (0 page "DOTS-RED-BLUE" #t)
    (2 mov "RED-1" RED 1)
    (2 mov "RED-2" RED 2)
    (2 mov "RED-3" RED 3)
    (1 mov "RED-4" RED 4)
    (1 mov "RED-5" RED 5)
    (1 mov "RED-6" RED 6)
    (2 mov "BLUE-1" BLUE 1)
    (2 mov "BLUE-2" BLUE 2)
    (2 mov "BLUE-3" BLUE 3)
    (1 mov "BLUE-4" BLUE 4)
    (1 mov "BLUE-5" BLUE 5)
    (1 mov "BLUE-6" BLUE 6)
    ;;
    ;; (0 page "DOTS-ORANGE-YELLOW" #t)
    ;; (2 mov "YELLOW-1" YELLOW 1)
    ;; (2 mov "YELLOW-2" YELLOW 2)
    ;; (2 mov "YELLOW-3" YELLOW 3)
    ;; (1 mov "YELLOW-4" YELLOW 4)
    ;; (1 mov "YELLOW-5" YELLOW 5)
    ;; (1 mov "YELLOW-6" YELLOW 6)

    (0 back "Distance Back" GREY #t 260 260 "Distance")
    ))

(define DIR-DECK
  #((0 deck "DirDeck" "dir-deck")	      ; name of object in js/ts
    (0 page "Direction")
    (2 dir "N")
    (2 dir "E")
    (2 dir "S")
    (2 dir "W")
    (2 dir "NW")
    (2 dir "NE")
    (2 dir "SE")
    (2 dir "SW")
    (0 back "Direction Back" GREY #t 260 260 "Direction")
    ))

(define DIR-DECK2
  #((0 deck "DirDeck2" "dir-deck2")	      ; name of object in js/ts
    (0 page "Direction")
    ;; old-style:
    (3 dir "N" "Blocked")
    (3 dir "E" "Blocked")
    (3 dir "S" "Blocked")
    (3 dir "W" "Blocked")
    (1 dir "NS" "Blocked")
    (1 dir "EW" "Blocked")

    (1 dir "NE" "Blocked")
    (1 dir "SE" "Blocked")
    (1 dir "SW" "Blocked")
    (1 dir "NW" "Blocked")

    (0 page "DirBack")			; page-eject, make back on clean sheet:
    (0 back "Direction Back" GREY #t 260 260 "Direction")
    ))

(define ROAD-DECK
  ;; name cost spec subtype (rotate/symmetric)
  ;; include in Policy/Event Deck
  #((0 deck "RoadDeck" "road-deck")	      ; name of object in js/ts
    (0 page "Road")
    (6 road "Express Lane" 2 (S S S S) (cardProps (onStep (payOwner (set rentAdjust)))))
					; if Lake/Park raises the Rent
    (4 road "LRLR" 2 (L R L R))
    (4 road "RLRL" 2 (R L R L))

    (3 road "All Left"  2 (L L L L))
    (3 road "All Right" 2 (R R R R))
    ;;
    (1 road "Merge Up S   " 3 (S R S L))
    (1 road "Merge Up L   " 3 (L R S L))
    (1 road "Merge Up R   " 3 (R R S L))
    (1 road "Merge Dn S   " 3 (S L S R))
    (1 road "Merge Dn L   " 3 (S L L R))
    (1 road "Merge Dn R   " 3 (S L R R))
    ;; Merge Lt == Merge Rt (just upside down)
    (1 road "Merge Lt S   " 3 (R S L S))
    (1 road "Merge Lt L   " 3 (R S L L))
    (1 road "Merge Lt R   " 3 (R S L R))
    (1 road "Merge Rt S   " 3 (L S R S))
    (1 road "Merge Rt L   " 3 (L L R S))
    (1 road "Merge Rt R   " 3 (L R R S))
    ))


(define ALIGN-DECK
  #((0 deck "AlignDeck" "align-deck")	; name of object in js/ts
    (1 road "Rotate-R" () (RR RR RR RR))
    (1 road "Rotate-L" () (RL RL RL RL))
    ))

(define MISC-ROADS
  #((0 deck "MiscRoads" "misc-roads")	      ; name of object in js/ts
    ;; Toroidial Alignments:
    (1 road "Rotate-R" () (RR RR RR RR))
    (1 road "Rotate-L" () (RL RL RL RL))

    (1 road "Diag-L" () (RL RR RL RR))
    (1 road "Diag-R" () (RR RL RR RL))

    (1 road "Asy-2" () (RR RL RL RR))
    (1 road "Asy-1" () (RR RL RR RR))

    (1 road "Vert Lt 2" 1 (S S L L))
    (1 road "Hori Rt 2" 1 (S S R R))
    (1 road "Hori Lt 2" 1 (S L L S))
    (1 road "Vert Rt 2" 1 (S R R S))

    (1 road "SLLL" 3 (S L L L))
    (1 road "SRRR" 3 (S R R R))
    (1 road "LSLL" 3 (L S L L)) ; rev-G

    (1 road "RSRR" 3 (R S R R)) ; G
    (1 road "LLLS" 2 (L L L S)) 

    (1 road "SRRL" 3 (S R R L))	; up, side, down [up down right]
    (1 road "LRLS" 3 (L R L S))	; Zig up 
    (1 road "RLRS" 3 (R L R S)) ; Zig Down 
    (1 road "LSLR" 3 (L S L R))	; 
    (1 road "RSRL" 3 (R S R L)) ; 
    ))

(define MISC-DECK
  #((0 deck "MiscDeck" "misc-deck")	      ; name of object in js/ts
    (0 page "Auction" #t)
    ;; RED ORANGE YELLOW GREEN BLUE PURPLE
    ;; (1 auction "N6" "+4" "+4" PURPLE) (2 own "WHITE" WHITE 0)
    ;; (1 auction "N5" "+3" "+2" BLUE)   (2 own "WHITE" WHITE 0)
    ;; (1 auction "N4" "+2" "+1" GREEN)  (2 own "WHITE" WHITE 0)
    ;; (1 auction "N3" "+1" "+0" YELLOW) (2 own "WHITE" WHITE 0)
    ;; (1 auction "N2" "+0" "+0" ORANGE) (2 own "WHITE" WHITE 0)
    ;; (1 auction "N1" "-1" "-1" RED)    (2 own "WHITE" WHITE 0)
    ;;
    (0 page "Owners" #t)		; Note: Owners is its own back....
    ;; (3 own "PURPLE" PURPLE 0)
    (3 own "RED" RED 0)
    ;; (3 own "GREEN" GREEN 0)
    (3 own "BLUE" BLUE 0)
    (3 own "ORANGE" ORANGE 0)
    ;; (3 own "YELLOW" YELLOW 0)
    ;;
    ))

(define TECH-DECK			; alternative TestDeck
  (expandify
  #((0 deck "TechDeck" "tech-deck")
    ;; (0 page "HighTech" #t)
    ;; Original (FAANG)
    ;; (high-tech "Facebook" "High Tech")	      ; commercial
    ;; (high-tech "Apple" "High Tech")	      ; commercial industrial
    ;; (high-tech "Amazon" "High Tech")	      ; commercial industrial
    ;; (high-tech "Netflix" "High Tech")	      ; commercial
    ;; (high-tech "Google" "High Tech")	      ; commercial
    ;; High Tech (w/o Trademarks)
    (high-tech "High Tech" "Social Network")  ; Facebook
    (high-tech "High Tech" "Shiny Devices")   ; Apple
    (high-tech "High Tech" "Online Shopping") ; Amazon
    (high-tech "High Tech" "Streaming Video") ; Netflix
    (high-tech "High Tech" "Internet Ads")    ; Google
    ;;(0 page "TileBack" #t) 		; page-eject, make back on clean sheet:
    ;;(10 back "Tile Back" GREY #t 260 260 "Tile")
    )))

(define TEST-DECK
  (expandify
  #((0 deck "TestDeck" "test-deck")
    (0 page "Test" #t)

    (1	home "Home"    0  1 0 1 RED)

    ;; (high-tech "High Tech" "Internet Ads")

    ;; (12 circle "House"	    "house" GREEN  2 () () 0.5 () (vp  1) (image "House0.png"))

    ;; (2 deferred "Demolition" BLUE
    ;;    "Pay $2 to discard an eligible* tile." ()
    ;;    "Discard on your next Draw.\n*not owned by other Player & VP = 0" ; Bring-Your-Own-Build; this card just brings the bulldozer
    ;;    (cardProps
    ;; 	(event (doUrbanRenewal 2))))	; set legalMark/buildCost on clickable Tiles

    ;; (1 road "Merge Lt L   " 3 (R S L L))
    ;; (1 road "Merge Lt R   " 3 (R S L R))
    ;; (1 road "Rotate-R" () (RR RR RR RR))
    ;; (1 road "Rotate-L" () (RL RL RL RL))
    ;;(1 road "Express Lane" 2 (S S S S) (cardProps (onStep (payOwner (set rent)))))

    (1 back "City-Back" GREY #t 260 260 "" (image "CitymapBack.png" 0 0))		; cityback
    )))

(define LAKE (mini-vec 0 TILE-DECK "Lake"))

(define NEWT (mini-vec 0 TILE-DECK "Builder" "Foundry" "Assembly" "Factory"
		       "Airport" "Plaza" "Enterprise Zone"))
(define BARS (mini-vec 0 TILE-DECK "Bar" "Restaurant" "Night Club" "Casino"))
(define BANKS (mini-vec 0 TILE-DECK "ATM" "Bank" "Brokerage" "Stock Market"))


;; Two mini-decks with deck-name for -with-file:
;; Note: these will overwrite the usual files!!!
(define TAX   (mini-vec 1 EVENT-DECK "EventDeck" "Income Tax" "Property Tax" "Wealth Tax"))
(define IND   (mini-vec 1 TILE-DECK "TileDeck" "Builder" "Foundry" "Assembly" "Factory"))

(define GOV   (mini-vec 1 TILE-DECK "Jail" "County Recorder" "Enterprise Zone" "City Hall" "Court House"))
(define AIR   (mini-vec 1 TILE-DECK "Airport"))
(define TAXI  (mini-vec 1 TILE-DECK "Taxi"))
(define TCOIN (mini-vec 1 TILE-DECK "Transit Hub"))
(define TDECK (mini-vec 1 TILE-DECK "County Recorder" "Jail" "City Hall"))
(define JAIL (mini-vec 0 TILE-DECK "Jail"))
(define BAR  (mini-vec 0 TILE-DECK "Bar"))
(define FIN  (mini-vec 0 TILE-DECK "Stock Exchange"))
(define CASINO (mini-vec 0 TILE-DECK "Casino"))

(define ROAD (mini-vec 0 POLICY-DECK "Road Repair EW" "Road Repair NS"))
(define DIST (mini-vec 0 POLICY-DECK "Adjust Distance 2"))
(define DOM  (mini-vec 0 EVENT-DECK "Eminent Domain"))
(define BACK (mini-vec 0 BACK-DECK "City-Back"))

(define DOT  (mini-vec 0 DOTS-DECK "PURPLE-5" "PURPLE-6"))
(define OWN  (mini-vec 0 DOTS-DECK "RED" "BLUE"))
(define FLUSH #((1 mov "PURPLE-0" PURPLE 0) (0 page "FLUSH" #t)))

(define HOME
  #((1  home "Home"    0  1 0 1 RED)
    (1  home "Home"    0  1 0 1 ORANGE)
    (1  home "Home"    0  1 0 1 YELLOW)
    (1  home "Home"    0  1 0 1 GREEN)
    (1  home "Home"    0  1 0 1 BLUE)
    (1  home "Home"    0  1 0 1 PURPLE)
    ))
(define HOUSING (mini-vec 0 HOME-DECK "Housing"))
(define HOUSE (mini-vec 1 HOME-DECK "House"))
(define TOWER (mini-vec 1 HOME-DECK "Tower")) ; HOME-DECK:Tower

;;; We can use est-deck to read/process Kate's mailing list: KateAddr/scripts/kate-addr.scm
;;; fill the template with many (set-text 'center ...) boxes.

(define (try-load-aux-proj aux-proj-name)
  (message-string1 "try load" (stringify aux-proj-name))
  (when (and aux-proj-name
	     (load aux-proj-name))	; no harm if fails
	;;(card-setup-aux)
	(let ((aux-spec (card-get-aux)))
	  (message-string1 "Aux info: " aux-spec)
	  (set! AUX-SPEC aux-spec)
	  (when (pair? AUX-SPEC)
		(set! AUX (nth 3 AUX-SPEC))
		(set! AUX-PROC (nth 2 AUX-SPEC))
		(template-use (nth 0 AUX-SPEC) (nth 1 AUX-SPEC)) ; not clear if this succeeds...
		))))
  

;;; GUI-invokable from GIMP: sf-make-info OR sf-make-deck (depending on do-gimp)
;;; independent of DO-TEMPLATE
(define (make-a-deck deck-str do-gimp)
  (try-load-aux-proj AUX-PROJ-NAME)
  (set! DO-GIMP do-gimp)		; #f: INFO-only, #t INFO & PNGs [controls ifgimp macro]

  (message-string1 "make-a-deck" deck-str do-gimp)
  (if (string-contains deck-str ":")
      ;; make a mini-vec with the requested card-spec, possibly reset nreps
      ;; and do not make a text file
      (let* ((display? (if (string-ends-with deck-str ":" "") #t CARD-DISPLAY?)) ; force DISPLAY
	     (deck-card (strbreakup deck-str ":")) ; ("EVENT-DECK" "School" ["0"]) 
	     ;;(kk (message-string1 "make-a-deck" deck-card))
	     (qq (if (< (length deck-card) 3) (set! deck-card (append deck-card (list "0")))))
	     (dname (nth 0 deck-card))
	     (cname (nth 1 deck-card))
	     (nreps (string->number (nth 2 deck-card)))
	     (sdeck (eval (string->symbol dname))) ; SYMBOL-DECK, SYMBOL-DECK:ALL, SYMBOL-DECK:CARD:3
	     (deck (if (equal? cname "ALL") sdeck (mini-vec nreps sdeck cname))))
	(card-set-page-name deck-str)
	(message-string1 "make-a-deck" dname cname nreps)
	;; with-card-display [without fluid-let]
	(let ((orig-display CARD-DISPLAY?))
	  (set! CARD-DISPLAY? display?)
	  (catch t (card-make-deck deck))
	  (set! CARD-DISPLAY? orig-display)))
      
      ;; ELSE just make the full deck:
      (cond
       ((equal? deck-str "ALL")
	(for-each card-make-deck-with-file
		  (list DOTS-DECK DIR-DECK ALIGN-DECK HOME-DECK		     ; special backs
			TILE-DECK EVENT-DECK POLICY-DECK TECH-DECK ROAD-DECK ; City-Back
			)))
       ((equal? deck-str "CARDS")
	(for-each card-make-deck-with-file
		  (list HOME-DECK					     ; special backs
			TILE-DECK EVENT-DECK POLICY-DECK TECH-DECK ROAD-DECK ; City-Back
			)))
       ((equal? deck-str "HORIZ")
	(for-each card-make-deck-with-file
		  (list EVENT-DECK POLICY-DECK)))
       (else
	(card-make-deck-with-file (eval (string->symbol deck-str)))))
      )
  )

(define (card-get-aux) (message-string1 "orig aux") #())
(define AUX-PROJ-NAME #f)		; path to aux script
(define AUX-SPEC #f)			; AUX-PROJ can override with (card-get-aux)
(define AUX-PROC #f)			; 
(define AUX #())			; AUX Deck

;;;;;;;;;
;;; TODO: try using parasite info to track image->display; script-fu-para-cards
(define (delete-all-images)
  (let* ((image-list (vector->list (cadr (gimp-image-list)))))
    (for-each (lambda (id)
		(message-string1 "try delete image" id)
		(catch (begin (message-string "skip image:" id))
		       (para-kill-display id)
		       (if (= TRUE (car (gimp-image-is-valid id))) (gimp-image-delete id)))
		)
	      image-list)
    ))


;;;;  MENU BINDINGS -- Key Bindings are done in GIMP! 
;;; C-M-a (display; C-M-d not available)
(define (sf-card-set-display-on)
  (message-string1 "set to display card.")
  (set! CARD-DISPLAY? #t))
;;; C-M-p (print)
(define (sf-card-set-display-off)
  (message-string1 "set to print & publish pages.")
  (set! CARD-DISPLAY? #f))

;;; C-M-i (info file)
(define (sf-make-info deck-str)
  (message-string "gimp-off: just produce cardinfo file:" deck-str)
  (make-a-deck deck-str #f))  

;;; C-M-m
(define (sf-make-deck deck-str)
  (make-a-deck deck-str #t))

;;; C-M-s
(define (sf-card-set-display display?)
  (if display?
      (sf-card-set-display-on)
      (sf-card-set-display-off)))

;;; C-M-k
(define (sf-card-stop-gen)
  (set! CARD-LOOP #f))

(define (sf-card-add-bleed image)
  ;;; if portrait?
  (card-add-bleed image 575 800 50 WHITE))

;;; C-M-t
(define (sf-to-template-nreps image drawable)
  ;;(message-string1 "N to template: " nreps)
  (card-do-template-nreps image (card-get-nreps image) #t #t))


;;; C-M-l ??
(define (sf-load-proj proj-name)
  ;; "proj/name" -> ("proj" "name")
  (message-string1 "sf-load-proj" (stringify proj-name))
  (let* ((names (strbreakup proj-name "/")))
    (set! AUX-PROJ-NAME (util-load-path (car names) (cadr names)))))

(sf-reg "sf-load-proj" "z-load-proj" "load aux scm file" "" SF-STRING "proj/file" "KateAddr/kate-addr")

(sf-reg "sf-make-info" "w-Info File" "all the cards" "" SF-STRING "Deck to Make" "EVENT-DECK")
(sf-reg "sf-make-deck" "w-Make Deck" "all the cards" "" SF-STRING "Deck to Make" "TILE-DECK")

(sf-reg "sf-card-add-bleed" "w-Add Bleed" "add bleed around this image"
	"RGB* GRAY*" SF-IMAGE "Current Image" 0)
;;	SF-DRAWABLE "Input Drawable (layer)" 0)

(sf-reg "sf-card-set-display" "w-Set-Display" "Toggle Display?" "" SF-TOGGLE "Set DISPLAY?" TRUE)
(sf-reg "sf-card-set-display-on" "w-Set-Display-On" "Display" "" )
(sf-reg "sf-card-set-display-off" "w-Set-Display-Off" "Print" "" )

;; C-M-k
(sf-reg "sf-card-stop-gen" "w-Cancel-Gen" "Stop Generation" "")

(sf-reg	 "sf-to-template-nreps" "w-nreps to Template" "set N times in template" "RGB* GRAY*"
	 SF-IMAGE "Input Image (current image)" 0
	 SF-DRAWABLE "Input Drawable (layer)" 0)

(sf-reg-file "delete-all-images"   "Del Images" "delete image if able" "" )


(gimp-message "est-deck loaded")

;;; Local Variables:
;;; eval: (make-variable-buffer-local 'backup-file-path)
;;; eval: (setq backup-file-path "~/Data/Programs/ng/citymap/src/app/cardinfo/est-deck.scm")
;;; eval: (defun backup-est-deck () (write-region (point-min) (point-max) backup-file-path))
;;; before-save-hook: backup-est-deck
;;; End:
