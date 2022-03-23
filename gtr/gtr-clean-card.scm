;; (gimp-image-pick-color image drawable x y merged? average? ave-radius)
;; (gimp-selection-value image x y) ==> int32 (0 if xy not in selection)
;; (gimp-image-select-item) vs combine
;; about 75 pixels in the border

;;(define clean "/Users/jpeck/Library/Application Support/GIMP/2.10/scripts/script-fu-clean-card.scm")

;; the registered entry point:
(define (script-fu-clean-card image drawable)
  (script-fu-clean-card-1 image drawable #t #f))

(define (script-fu-clean-card-debug image drawable)
  (script-fu-clean-card-1 image drawable #f #f))

;; use offsets for new style cards:
(define (script-fu-clean-card-new image drawable)
  (script-fu-clean-card-1 image drawable #t #t))

(define (script-fu-clean-card-1 image drawable undo newp)
  ;;(gimp-message-set-handler MESSAGE-BOX)   ; 0
  ;;(gimp-message-set-handler CONSOLE)       ; 1
  ;;(gimp-message-set-handler ERROR-CONSOLE) ; 2
  ;;(gimp-message-set-handler 2)
  (clean-card image drawable undo #t newp)
  )

;; true means use 'newtype' corrections; set in get-filetype
(define *NEWTYPE* #f) 
  
(define (clean-card image drawable undo context newp)
  (set! *NEWTYPE* newp)
  (message-string "====================================" newp)
  (let* ((layer (car (gimp-image-get-active-layer image)))
	 (xxx (message-string "layer=" layer "drawable=" drawable))
         (color (pick-border-color image layer))
	 (bot-offset 0)
         )
    (and undo (gimp-image-undo-group-start image)) ; mark for undo
    (and context (gimp-context-push))              ; save FG color, etc
    (and context (gimp-context-set-defaults))

    (gimp-layer-add-alpha layer)
    (set! bot-offset			      ;when (mul 8) > orig-height
	  (trim-to-rough-bounds image layer)) ;rr with color corner; size (mul 8)
    (message-string "bot-offset1=" bot-offset)
    (set! bot-offset
	  (- bot-offset (crop-to-size image layer 744 1048)))
    (message-string "bot-offset2=" bot-offset)

    (clean-edge-pixels image layer color bot-offset)

    (and context (gimp-context-pop))
    (and undo (gimp-image-undo-group-end image))

    ;; in debug mode, do not auto-add bleed layer
    (and undo
	 (script-fu-add-bleed image layer color))
    
    (gimp-displays-flush)
    )
  )



  ;; enter with 'rough-selection' (contiguous color bounds)
  
  ;; What we *want* to do is remove 'black' that is exterior to 'color' [blue]
  ;; Best to have a 'path-operator' to clear-ify black that have a color neighbor.
  ;; (or: ADD contiguous for each pixel from "edge-3" )
  ;; Here we shrink the color-derived *outline* to pick up black edge pixels.
  ;; Sadly that also finds graphic bits that are on the edge.

  ;; clean up the edges:
  ;; for pixel (n from edge: n=0)
  ;; if the pixel 5 steps "in" is like the background
  ;; and this pixel is NOT like background
  ;; then set this pixel to background, and fix (n=n+1, unless n> 5)
  ;; NOTE: "edge" pixel is first non-transparent pixel
  ;; if that is not the first pixel, then assume corner and do the trig
    

;; assert: selection = "border" a mostly rounded rectangle
;; assert: selection-bounds == image-bounds (crop-to-bounds)
(define (clean-edge-pixels image layer color bot-offset)
  (message-string 'clean-edge-pixels 'bot-offest3= bot-offset)
  (let* ((offsets (gimp-drawable-offsets layer))
         (x0 (nth 0 offsets))		; pro'ly = 0
	 (y0 (nth 1 offsets))		; pro'ly = 0
	 (w0 (car (gimp-drawable-width layer)))
	 (h0 (car (gimp-drawable-height layer)))

         (dgrow 2)
         (dsize 8)
         (BLACK '(0 0 0))
         (WHITE '(255 255 255))
	 (protects (edge-graphics-protects image bot-offset))
         (crop (car (gimp-image-get-channel-by-name image "crop")))
	 (outer 0)
	 (outer-final 0)
	 (layer2 (car (gimp-layer-copy layer FALSE))) ;don't ADD alpha..
	 )
    ;;(gimp-image-insert-layer image layer2 0 1)
    (gimp-image-insert-layer image layer2 0 -1) ;later: lower-to-bottom
    (message-string 'image image 'layer layer 'layer2 layer2)

    ;; need to mask out the protects! (- crop protects)
    (gimp-selection-none image)
    (select-items image CHANNEL-OP-ADD protects "c-e-p protects1") ;ADD 
    (fill-selection image layer2 color) ; color to join for fill-color

    ;; backfill from *8 resize:
    (gimp-selection-invert image)
    (when (> crop 0)
	  (gimp-image-select-item image CHANNEL-OP-INTERSECT crop)
	  )
    (fill-selection image layer2 BLACK)
    
    ;; wrap-around contiguous space; and room for selection-border continuity
    (resize-and-fill image layer2 dsize BLACK)	;new area selected & filled

    ;; note: "layer" is still original size
    (gimp-layer-resize-to-image-size layer)

    ;; select by color on the modified layer:
    (select-by-color image layer2 60 '((2 2))) ;REPLACE all the BLACK
    (fill-selection image layer2 WHITE)
    (select-by-color image layer2 60 '((2 2)) CHANNEL-OP-ADD)
    (set! outer (save-selection-as image "outer")) ;outer bits

    (gimp-image-lower-item-to-bottom image layer2)

    (gimp-selection-invert image)	;select layer/image perimeter
    (gimp-selection-grow image dgrow)
    (gimp-selection-border image dgrow)	;smoothes outlier pixels
    (gimp-image-select-item image CHANNEL-OP-ADD outer)
    (select-items image CHANNEL-OP-SUBTRACT protects "c-e-p protects2")
    (set! outer-final (save-selection-as image "outer-final"))

    ;; fill-border while there is connectivity via outer-resize:
    (fill-selection image layer color)
    (message-string "color resize area:" color)
    (refill-border image layer2 protects layer color dsize)

    ;; clean edge shine
    (gimp-image-select-item image CHANNEL-OP-REPLACE outer-final)
    (gimp-selection-border image 4)
    (select-items image CHANNEL-OP-SUBTRACT protects "c-e-p: protects3")
    (fill-selection image layer color)

    ;;
    (gimp-image-select-item image CHANNEL-OP-REPLACE outer-final)
    (gimp-drawable-edit-clear layer2)
    (gimp-drawable-edit-clear layer)
    ;; maybe: (fill-selection image layer color) ??

    ))

(define (edge-graphics-protects image dsize)
  (let* ((imageh (car (gimp-image-height image)))
         (type (get-filetype image))
	 ;; not used? feed to (pick-border-color ..)?
	 (colors '(  ("Odd" 26 26) 	;SITE
		     ("Yellow" 208 140)	;fced0d
                     ("Brown" 18 152)
                     ("Blue" 80 113)
                     ("Purple" 100 340) ;7d387f
                     ("Grey" 210 110 )	;7b7c76 afb0b0
                     ("Red" 140 214) )) ;e30606

         (corners '( ("Odd" 900 1000)	;stripe
		     ("Yellow" 208 140) ; Y values
                     ("Brown" 28 146)
                     ("Blue" 80 123)
                     ("Purple" 100 340)
                     ("Grey" 210 110 )
                     ("Red" 120 234) ))
	 (bottoms '( ("Odd" 8 740)
		     ("Yellow" 205 368)	; X values
		     ("Brown" 270 300)
		     ("Blue" 173 402)
		     ("Purple" 194 405)
		     ("Grey" 190 385)
		     ("Red" 210 334)))
         (coryl (assoc type corners))
	 (botxl (assoc type bottoms))
         ;;(xx (message-string "coryl=" coryl))
         (cory (if (pair? coryl) (cdr coryl) '(18 422)))
         (botx (if (pair? botxl) (cdr botxl) '(173 422)))

         (cor-x 0)
         (cor-w 8)
	 (bot-h 16)
	 (kkk (message-string 'imageh imageh 'bot-h bot-h 'dsize dsize))
         (bot-y (- imageh bot-h dsize))	   ;align to bottom of image
         (yyy (gimp-image-select-rectangle ;protect corner graphic:
                image CHANNEL-OP-REPLACE cor-x (car cory) cor-w (cadr cory)))
         (zzz (gimp-image-select-rectangle ;protect bottom graphic:
                image CHANNEL-OP-ADD (car botx) bot-y (cadr botx) bot-h))
         (protects (save-selection-as image "protects"))
         )
    ;; return arg lists for (gimp-image-select-item )
    (list protects)
    ))

(define (select-items image OP items . msg)
  (gimp-image-undo-group-start image)
  (message-string "select-items:" items msg)
  (while (and (pair? items))
	 (let ((args (append (list image OP) (list (car items)))))
	   ;;(message-string "select-item:" args)
	   (apply gimp-image-select-item args)
	   (set! items (cdr items))))
  (gimp-image-undo-group-end image)
  )

(define (trim-to-rough-bounds image layer)
  (trim-to-rough-card image layer 90 35 #f) ; drawable=layer
  ;; crop = -orig -rrect [exterior of card: BLACK]
  ) 

(define (trim-to-rough-card image layer thresh r0 crop-to-8)
  ;; find card, cut-to-bounds:
  ;; approx 750 x 1050 (2.5 x 3.5) [8 * (92, 130) = 736, 1040]
  ;; or...: 736 x 1036 [14 pixel short]
  (select-by-color image layer thresh '((145 28) (28 1016)))
  ;; select all the outer border-color...
  (let* ((orig (save-selection-as image "orig"))
	 (bounds (gimp-selection-bounds image))
	 (x0 (nth 1 bounds))
	 (y0 (nth 2 bounds))
	 (w0 (- (nth 3 bounds) x0))
	 (h0 (- (nth 4 bounds) y0))
	 (rr (+ r0 0))	;; shrink corners; avoid selecting corner pixels
	 (crop 0)
	 (m8 (if crop-to-8 8 1))
	 (x8 (* m8 (floor (/ x0 m8))))
	 (y8 (* m8 (floor (/ y0 m8))))
	 (w8 (* m8 (ceiling (/ (+ w0 (- x0 x8)) m8))))
	 (h8 (* m8 (ceiling (/ (+ h0 (- y0 y8)) m8))))
	 (bot-offset (- h8 h0))
	 (protects 0)
	 )
    ;; resize to mul-8 that is > size of selected pixels
    ;; generally x8,y8 = 0; w8,h8 may grow to mul-8
    (when (and crop-to-8 (or (> w8 w0) (> h8 h0)))
	  (message-string "trim-to: " w8 h8 "from" w0 h0)
	  (gimp-image-resize image w8 h8 0 0) ;assert offset==0
	  (gimp-layer-resize-to-image-size layer))
    (if #t
	;; 
	(begin
	  ;; if there are pixels outside of selection; black them.
	  (gimp-selection-invert image) ; orig -> xorig
	  (gimp-image-select-round-rectangle
	   image CHANNEL-OP-SUBTRACT x0 y0 w0 h0 rr rr) ;use fuzzy corners!
	  ;; selecting all the edge cruft [or nil, if no edge stuff]
	  (fill-selection image layer '(0 0 0))
	  ;; return with image cropped to [RR] selection bounds
	  ;; return the saved channel with the base color
	  (set! crop (save-selection-as image "crop"))
	  ;; return crop
	  ))
    (gimp-image-crop image w8 h8 x8 y8) ; crop to selection (mul 8)
    bot-offset
    ))

(define (offset8 w w8 . dk)
  ;; w8 is the current mul-8 dimension of layer
  ;; w is the desired dimension [presumably also mul-8]
  ;; remove small bits from right
  ;; when 16 or more offest to remove from left
  (let* ( ;;  -1 for: x0 = 0 8 8 16 17
	 (k (if (pair? dk) (car dk) 0))
	 (xx (- w8 w))
	 (dx (floor (/ xx 8))) ; 0 1 2 3 ...
	 (x0 (* 8 (floor (/ (+ k dx) 2)))) ; 0 0 8 8 16
	 )
    x0))

(define (crop-to-size image layer w h)
  ;; 744 x 1048 = 8*(93, 131) < 2.5x3.5 * 300 < 752 x 1056 = 8*(94, 132)
  ;; 2.5x3.5 == 750 x 1050 (mul-8 up to 752 x 1056)
  (let* ((db 0)
	 (w8 (car (gimp-drawable-width layer)))
	 (h8 (car (gimp-drawable-height layer))))
    (message-string "cts:" "w=" w "h=" h "w8=" w8 "h8=" h8)
    (if (or (< w w8) (< h h8))
	(let* ((x0 (offset8 w w8))
	       (y0 (offset8 h h8)))
	  (message-string "cts-crop:" (min w w8) (min h h8) x0 y0)
	  (gimp-image-crop image (min w w8) (min h h8) x0 y0)
	  (when (or (< w8 w) (< h8 h))
		(message-string "CARD TOO SMALL: " 'w8 w8 'h8 h8)
		(message-string "MAYBE USE gimp-image-resize")
		)
	  (set! db (- h8 (min h h8)))
	  ))
    db))


;; drawable is the main layer:
;; ds is offest of image-canvas from scanned card size
(define (refill-border image layer2 protects layer color ds)
  ;; fill 75 pixels of Border with pure color:
  (let* ((dk 75)
	 (dkw (* 2 300))
	 (dkh (* 3 300))
	 (nthresh 40)
	 (seed0 '((145 28) (27 840) )) ;(34 846) (34 1016)(34 546)
	 (seeds ()); offset seed0 by ds
	 (loopr (while (pair? seed0)
		       (let* ((xy (car seed0))
			      (nxy (list (+ ds (car xy))
					 (+ ds (cadr xy)))))
			 (set! seeds (cons nxy seeds))) ;push nxy on list
		       (set! seed0 (cdr seed0))))
	 (offsets (gimp-drawable-offsets layer2))
	 (x0 (nth 0 offsets))		; pro'ly = 0
	 (y0 (nth 1 offsets))		; pro'ly = 0
	 (w0 (car (gimp-drawable-width layer2)))
	 (h0 (car (gimp-drawable-height layer2)))
	 (xf (if *NEWTYPE* 16 0)) ; 16-bit fudge for new cards (Solarium)
	 )
    (message-string "refill-border:seeds=" seeds "ds=" ds)
    ;;(gimp-selection-none image)
    ;; only one 'item' in protects:
    (select-items image CHANNEL-OP-REPLACE protects "refill-border:protects")
    (fill-selection image layer2 '(0 0 0)) ; BLACK

    (select-by-color image layer2 nthresh seeds) ;assumed to be in border
    (save-selection-as image "orig-border")
    (gimp-image-select-rectangle
     image CHANNEL-OP-SUBTRACT (+ x0 dk ds xf) (+ y0 dk ds) dkw dkh)

    (save-selection-as image "border")
    (fill-selection image layer color) ; fill on orig layer
    (message-string "refill-border: " w0 "X" h0)
    ))

(define (get-filetype image)
  (let* ((filename (car (gimp-image-get-filename image)))
         ;;(x1 (gimp-message filename))
         (slash (strbreakup filename DIR-SEPARATOR))
         ;;(x2 (gimp-message (car slash)))
         (rslash (reverse slash))
         ;;(x3 (gimp-message (car rslash)))
         (name (car rslash))		; "Type-012.jpg"
         ;;(x4 (gimp-message name))
	 (name2 (strbreakup name "-"))
         (type (car name2)) ; "Type" "012.jpg"
         ;;(x5 (message-string "filetype=" type))
         )
    (message-string "filetype:" name "type=" type "new=" *NEWTYPE*)
    type))

;; we use layer as drawable...
(define (fill-selection image drawable fill-color)
  ;; put fill color in context-foreground and edit-fill
  ;; restore context
  (gimp-image-undo-group-start image)
  (if (equal? TRUE (car (gimp-selection-is-empty image)))
      (message-string "fill-selection: NO SELECTION"))
  ;; else:
  (gimp-context-push)
  (gimp-context-set-defaults)
  (gimp-context-set-antialias FALSE)
  (message-string "fill:" drawable fill-color)
  (gimp-context-set-foreground fill-color)      ; 
  (gimp-drawable-edit-fill drawable FILL-FOREGROUND)
  (gimp-context-pop)
  ;;)
  (gimp-image-undo-group-end image)
  )

(define (save-selection-as image name)
  ;; restore active layer
  (gimp-image-undo-group-start image)
  (let* ((layer (car (gimp-image-get-active-layer image)))
         (chan (car (gimp-selection-save image))))
    (gimp-channel-set-name chan  name)
    (gimp-image-set-active-layer image layer)
    (gimp-image-undo-group-end image)
    chan))

(define (pick-border-color image drawable)
  ;; sample color at point of the card using:
  ;; (image drawable x y merged-composite? sample-average? radius-to-average)
  (car (gimp-image-pick-color image drawable 150 25 FALSE TRUE 8))
  ;;(car (gimp-image-pick-color image drawable 25 1010 FALSE TRUE 15))
  )

(define (select-by-color image drawable threshold points . OP)
  ;; as determined by the threshold and criterion context values
  ;; (gimp-image-select-contiguous-color image OPERATION drawable x y)
  ;; Selection Operation:  {ADD=0, SUB=1, REPLACE=2, INTERSECT=3}

  (gimp-context-push)
  (gimp-context-set-defaults)
  (gimp-context-set-antialias FALSE)

  (gimp-context-set-sample-merged FALSE)
  (gimp-context-set-sample-criterion SELECT-CRITERION-COMPOSITE) ; R,G,B...
  (gimp-context-set-sample-threshold-int threshold)
  (gimp-context-set-sample-transparent FALSE)

  (when (not (pair? OP))
	(set! OP (list CHANNEL-OP-ADD))
	(gimp-selection-none image))
  (while (pair? points)
	 (let* ((point (car points))
		(x (car point))
		(y (cadr point)))
	   (set! points (cdr points))
	   (gimp-image-select-contiguous-color image (car OP) drawable x y)
	   ))
  
  (gimp-context-pop)

  ;; This procedure is affected by the following context setters:
  ;; 'gimp-context-set-antialias',
  ;; 'gimp-context-set-feather',
  ;; 'gimp-context-set-feather-radius',
  ;; 'gimp-context-set-sample-merged', (merge drawable with visible layers)
  ;; 'gimp-context-set-sample-criterion', (how similarity is determined)
  ;;       [ COMPOSITE R G B H S V A LCH-L LCH-C LCH-H ]
  ;; 'gimp-context-set-sample-threshold', (0--1 -int 0-255)
  ;; 'gimp-context-set-sample-transparent' (include "transparent" as selectable)
  )


;; helper for cmdline: image extend-by-nw from-given-drawable-layer
(define (resize image nw d)
  (let* ((w (car (gimp-drawable-width d)))
         (h (car (gimp-drawable-height d))))
    (gimp-image-undo-group-start image)
    (gimp-image-resize image (+ w nw nw) (+ h nw nw) nw nw)
    (gimp-image-undo-group-end image)
    ))

(define (resize-and-fill image layer nw fill-color)
  ;; context transform-resize: [ADJUST CLIP CROP CROP-WITH-ASPECT]
  ;; returns selection holding the new "border" filled with 'fill-color'
  (gimp-selection-all image) ; mark pre-resize area
  
  (resize image nw layer)
  (gimp-layer-resize-to-image-size layer)

  ;; and fill:
  (gimp-selection-invert image)
  (fill-selection image layer fill-color)
  )

(define (message-string . args)
  (gimp-message (stringify args)))

(define (get-chan-by-name image name)
  ;; use: gimp-image-get-channel-by-name
  (car (gimp-image-get-channel-by-name image name)))


(define (clean-edge-2 image drawable)
  ;; walk around the edge of image, rewrite pixels as necessary
  ;; we would rather have "Stroke Selection" do this
  ;; (gimp-drawable-edit-stroke-selection drawable)
  ;; but have not found how to insert a custom version of "Paintbrush"
  ;; (gimp-list-paint-methods) is not extensible?
  (define (stroke-selection-with-operator image oper)

    )
  (define del 5)
  (define xmin 0)
  (define xmax 999)
  (define ymin 0)
  (define ymax 999)
  ;; (gimp-selection-value image x y) ==> INT32 (0 <= value <= 255) !! no RGBA?
  ;; OH! Does this just tell you if a point is selected??

  ;; to get from corner point to test-sample:
  (define (inset dx dy del)
    (let* ((xx (* del dy (/ 1 (+ dx dy))))
           (yy (* del dx (/ 1 (+ dx dy))))
           (k2 (/ (* del del) (+ (* xx xx) (* yy yy))))
           (ks (sqrt k2))
           (rx (* xx ks))
           (ry (* yy ks))
           )
      (list rx ry)
      ))


  ;; offsets from anywhere on the edge to find test-sample point
  (define (which-edge x y)
    (let* ((d 2)
           (ctx (/ (+ xmin xmax) 2))
           (cty (/ (+ ymin ymax) 2))
           )
      (or (and (< x (+ xmin d)) '(del 0))  ;LEFT
          (and (< y (+ ymin d)) '(0 del))  ;TOP
          (and (> x (- xmax d)) '(-del 0)) ;RIGHT
          (and (> y (- ymax d)) '(0 -del)) ;BOTTOM
          (let ((dxl (- x xmin))
                (dxr (- x xmax))
                (dyt (- y ymin))
                (dyb (- y ymax)))
            (and (< x ctx) (< y cty) (inset dxl dyt)) ;UL)
            (and (< x ctx) (> y cty) (inset dxl dyb)) ;LL)
            (and (> x ctx) (< y cty) (inset dxr dyt)) ;UR)
            (and (> x ctx) (> y cty) (inset dyr dyb)) ;LR)
            )
          '(0 0))))

  (define (process-pixel x y)
    (let ((dxdy (which-edge x y)))
      
      )
    )
  ;; hmm what can we do with (gimp-selection-border image 5)?
  ;; selection INTERSECT (gimp-selection-border image 5)
  ;; selects the
  )

;; export image/drawable/layer for cmdline
(define (define-image i)
  (let* ((d (car (gimp-image-get-active-drawable i)))
         (l (if (gimp-item-is-layer d) d nil)))
    (gimp-layer-add-alpha l)
    ;;(define image i)
    ;;(define drawable d)
    ;;(define layer l)
    (list
     (list 'define 'image i)
     (list 'define 'drawable d)
     (list 'define 'layer l))
    )
  )

(define (get-drawable image) (gimp-image-get-active-drawable image))


(script-fu-register
 "script-fu-clean-card" "Clean Card" "script-fu cleanup"
 "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31" "RGB* GRAY*"
 SF-IMAGE "Input Image (current image)" 0
 SF-DRAWABLE "Input Drawable (layer)" 0
 ;;SF-COLOR "Border Color" '(100 100 100)
 )

;;(script-fu-menu-register "script-fu-clean-card" "<Image>/Script-Fu")

(script-fu-register
 "script-fu-clean-card-debug" "Clean Card - Debug" "script-fu cleanup"
 "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31" "RGB* GRAY*"
 SF-IMAGE "Input Image (current image)" 0
 SF-DRAWABLE "Input Drawable (layer)" 0
 ;;SF-COLOR "Border Color" '(100 100 100)
 )

;;(script-fu-menu-register "script-fu-clean-card-debug" "<Image>/Script-Fu")

(script-fu-register
 "script-fu-clean-card-new" "Clean Card - new" "script-fu cleanup"
 "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31" "RGB* GRAY*"
 SF-IMAGE "Input Image (current image)" 0
 SF-DRAWABLE "Input Drawable (layer)" 0
 ;;SF-COLOR "Border Color" '(100 100 100)
 )

;;(script-fu-menu-register "script-fu-clean-card-new" "<Image>/Script-Fu")

(gimp-message "loaded gtr-clean-card.scm")
