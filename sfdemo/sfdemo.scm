(define (script-fu-demo image drawa)
  (define (bleed-by-fill image drawa nw nh fill-color)
    (let* ((OP CHANNEL-OP-REPLACE)
	   (dist 254)
	   (seed (list (/ nw 2) (/ nh 2)))
	   )
      (message-string "select-by-color" image drawa dist seed OP)
      (select-by-color image drawa dist (list seed) OP)
      ;;(gimp-selection-flood image)
      (gimp-selection-sharpen image)
      (gimp-selection-invert image)
      (message-string "fill-selection" image drawa fill-color)
      ;;(fill-selection image drawa fill-color)
      (gimp-context-set-foreground fill-color)
      (gimp-drawable-edit-fill drawa FILL-FOREGROUND)
      
      drawa
      )
    )
  (let* ((fill-color '(50 200 50))	; GREEN
	 (nw (car (gimp-drawable-width drawa)))
         (nh (car (gimp-drawable-height drawa))))
    (bleed-by-fill image drawa nw nh fill-color))
  (gimp-displays-flush)
  )  
  
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

(script-fu-register
 "script-fu-demo" "Demo" "demo merge"
 "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31" "RGB* GRAY*"
 SF-IMAGE "Input Image (current image)" 0
 SF-DRAWABLE "Input Drawable (layer)" 0
 )

(script-fu-menu-register "script-fu-demo" "<Image>/Script-Fu")
