;;;;;;;;;
;;; add/remove bleeding edge:
;;;

(define (script-fu-add-bleed-debug image drawable . color)
  (add-bleed-method image drawable #f color))  

(define (script-fu-add-bleed image drawable . color)
  (add-bleed-method image drawable #t color))

(define (add-bleed-method image drawable undo color)
  ;; extend image to full template size, filling with bleed color
  (if (pair? color)
      (set! color (car color))
      (set! color (pick-border-color image drawable)))
  (message-string 'add-bleed-method image drawable undo color)

  (and undo (gimp-image-undo-group-start image)) ; mark for undo
  (and undo (gimp-context-push))		 ; save FG color, etc
  (and undo (gimp-context-set-defaults))

  ;; Template is (808x1096) 8*(101,137)
  ;; 808 x 1108
  ;;(add-bleed-impl image drawable  (* 8 101) (* 8 137) color)
  ;;(add-bleed-impl image drawable  (* 8 101) 1108 color 70)
  (let ((p? (> (car (gimp-image-height image)) (car (gimp-image-width image)) ))
	(w (apply (if p? min max) TEMPLATE-CARD-SIZE))
	(h (apply (if p? max min) TEMPLATE-CARD-SIZE)))
    (add-bleed-impl image drawable w h color 70))
  (and undo (gimp-context-pop))
  (and undo (gimp-image-undo-group-end image))
  (gimp-displays-flush)
  )

(define (add-bleed-impl image drawa nw nh color rr)
  (define (mid-offset nw w)
    (if (> nw w) (/ (- nw w) 2) 0))

  ;; crop or expand to fit new size:
  (define (bleed-resize image nw nh w h)
    (when (or (> w nw) (> h nh))
	  ;; crop to size:
	  (let* ((x0 (mid-offset w nw))
		 (y0 (mid-offset h nh)))
	    (message-string "bl-dnsize" w h nw nh x0 y0 )
	    (gimp-image-crop image nw nh x0 y0)
	    ))
    (when (or (< w nw) (< h nh))
	  ;; make bigger:
	  (let* ((x0 (mid-offset nw w))
		 (y0 (mid-offset nh h)))
	    (message-string "bl-upsize:" w h nw nh x0 y0 )
	    (gimp-image-resize image nw nh x0 y0)))

    (message-string "bl-resize:" drawa nw nw (/ (- nw w) 2) (/ (- nh h) 2))
    ;;(gimp-layer-resize drawa nw nh (/ (- nw w) 2) (/ (- nh h) 2))
    (gimp-layer-resize-to-image-size drawa)
    ;; old layer is centered in new size
    )

  (define (bleed-by-fill image drawa nw nh fill-color)
    (let* ((OP CHANNEL-OP-REPLACE)
	   (dist 254)
	   (seed (list (/ nw 2) (/ nh 2)))
	   )
      (message-string "select-by-color" image drawa dist seed OP)
      (select-by-color image drawa dist (list seed) OP)
      (gimp-selection-flood image)
      (gimp-selection-sharpen image)
      (gimp-selection-invert image)
      (when (= (car (gimp-selection-is-empty image)) FALSE)
	    (message-string "fill-selection" image drawa fill-color)
	    (fill-selection image drawa fill-color))
      drawa
      )
    )

  (define (hide-lower-layers image layer)
    (let* ((p (car (gimp-image-get-item-position image drawa)))
	   (layers (gimp-image-get-layers image))
	   (active (car layers))
	   (layerv (cadr layers))
	   (length (vector-length layerv))
	   (layern (+ 1 active)))
      (while (< layern length)
	     (gimp-item-set-visible (vector-ref layerv layern) FALSE)
	     (set! layern (+ 1 layern))))
    )
  (define (hide-other-layers image layer)
    (let* ((p (car (gimp-image-get-item-position image drawa)))
	   (layers (gimp-image-get-layers image))
	   (active (car layers))
	   (layerv (cadr layers))
	   (length (vector-length layerv))
	   (layern 1))
      (while (< layern length)
	     (when (not (= (vector-ref layerv layern) layer))
		   (gimp-item-set-visible (vector-ref layerv layern) FALSE))
	     (set! layern (+ 1 layern))))
    )

  (let* ((w (car (gimp-drawable-width drawa)))
         (h (car (gimp-drawable-height drawa))))
    (bleed-resize image nw nh w h)
    
    (let ((merge
	   (bleed-by-fill image drawa nw nh color)))
      ;; round the corners:
      (gimp-image-select-round-rectangle
       image CHANNEL-OP-REPLACE 0 0 nw nh rr rr)
      (gimp-selection-invert image)
      (gimp-drawable-edit-fill merge FILL-TRANSPARENT)
      (hide-other-layers image merge))
    )
  )

;;; maybe obsolete, can just add-bleed-impl to desired size?
;;; this has bunch of .jpg 8-pixel boundary stuff...
;;; offset8 in s-f-clean-card.scm
(define (script-fu-remove-bleed image layer)
  (if #f
      (old-remove-bleed image layer)
      (let ((nw 752) (nh 1056))
	(add-bleed-impl image layer nw nh '(0 0 0) 30)))
  (gimp-displays-flush))

(define (old-remove-bleed image layer)
  (let* ((w0 (car (gimp-image-width image)))
         (h0 (car (gimp-image-height image)))
	 ;;(x0 0) (y0 0)
	 ;; assuming w0>nw, h0>nh
	 (nw 752) (nh 1056)
	 (nx (/ (- w0 nw) 2))
	 (ny (/ (- h0 nh) 2))
	 ;; round to mul-8... from [nx,ny,nw,nh]
	 (x8 (* 8 (floor (/ nx 8))))
	 (y8 (* 8 (floor (/ ny 8))))
	 (w8 (* 8 (ceiling (/ (+ nw (- nx x8)) 8))))
	 (h8 (* 8 (ceiling (/ (+ nh (- ny y8)) 8))))
	 ;;
	 (x0 (offset8 nw w0 2))
	 (y0 (offset8 nh h0))
	 (rr 20)
	 )
    ;;(crop-to-size image layer nw nh)
    (gimp-image-select-round-rectangle
     image CHANNEL-OP-REPLACE x0 y0 nw nh rr rr)
    ;;(gimp-image-crop image w8 h8 x8 y8) ; crop to selection (mul 8)
    (gimp-selection-invert image)
    (fill-selection image layer '(0 0 0))
    (gimp-displays-flush)    
  ))

(script-fu-register
 "script-fu-add-bleed" "x-Add Bleed" "script-fu for GtR"
 "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31" "RGB* GRAY*"
 SF-IMAGE "Input Image (current image)" 0
 SF-DRAWABLE "Input Drawable (layer)" 0
 ;;SF-COLOR "Border Color" '(100 100 100)
 )

(script-fu-menu-register "script-fu-add-bleed" "<Image>/Script-Fu")

(script-fu-register
 "script-fu-add-bleed-debug" "x-Add Bleed - Debug" "script-fu for GtR"
 "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31" "RGB* GRAY*"
 SF-IMAGE "Input Image (current image)" 0
 SF-DRAWABLE "Input Drawable (layer)" 0
 ;;SF-COLOR "Border Color" '(100 100 100)
 )

(script-fu-menu-register "script-fu-add-bleed-debug" "<Image>/Script-Fu")


(script-fu-register
 "script-fu-remove-bleed" "x-Remove Bleed" "script-fu for GtR"
 "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31" "RGB* GRAY*"
 SF-IMAGE "Input Image (current image)" 0
 SF-DRAWABLE "Input Drawable (layer)" 0
 )

(script-fu-menu-register "script-fu-remove-bleed" "<Image>/Script-Fu")

