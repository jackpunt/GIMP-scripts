;;;; strbreakup/unbreakupstr are in:
;;; gimp/plug-ins/script-fu/scripts/script-fu-compat.init

(define (script-fu-save-clean image drawable)
  (define imgfile (car (gimp-image-get-filename image)))
  ;; NOTE all the jpg is obsolete, we only save to PNG!
  ;; sometimes we are editing a .xcf so replace with .jpg
  (define fnames (get-clean-name imgfile "png")) ; was "jpg"
  (define cname (car fnames))
  (define oname (cadr fnames))
  (message-string "oname: " oname)
  (define comment (string-append "clean-card version of " oname))
  (let* ((layers (gimp-image-get-layers image))
	 (toplayer (vector-ref (cadr layers) 0)))
    (message-string "save PNG: " cname oname)
    (file-png-save-defaults RUN-NONINTERACTIVE image toplayer cname oname))
  )

(define (old-jpg image toplayer cname oname comment)
  (let ((INTEGER 0)(FIXED 1)(FLOAT 2)
	)
    (if #f
	(file-jpeg-save RUN-NONINTERACTIVE image toplayer
			cname oname
			;; quality, smoothing, optimize, progressive
			.93 0.05 TRUE TRUE 
			comment
			2 ; 4:4:4 (no subsampling)
			TRUE ; baseline
			0 ; interval of restart markers
			FLOAT ; DCT method {INTEGER, FIXED, FLOAT}
			))
    ))

;;; returns name for "clean" image, in /clean directory
;;; (clean-name name-suf)
;;; if no /clean/ then use ../clean
(define (get-clean-name filename . sufl)
  (define rslash (reverse (strbreakup filename DIR-SEPARATOR)))
  ;;(message-string (car rslash))
  (define name (car rslash))
  (define name-suf (if (pair? sufl) (set-suffix name (car sufl)) name))
  (message-string "using name-suf= " name-suf)

  ;; add a "clean" subdirectory if there not already there
  (let* ((DIR DIR-SEPARATOR)
	 (dirname "clean")
	 (targetd (string-append DIR dirname DIR)))
    (if (not (string-contains filename targetd))
	(set! rslash (append (list name-suf dirname) (cddr rslash))))
    targetd
    )
  (define rnames (reverse rslash))
  (define clean-name (unbreakupstr rnames DIR-SEPARATOR)) ;; "/"
  (list clean-name name-suf)
  )

(define (set-suffix name suf)
  (if (null? suf)
      name
      (let ((rnamedot (reverse (strbreakup name "."))))
	(set-car! rnamedot suf)
	(unbreakupstr (reverse rnamedot) ".")))
  )


(script-fu-register
 "script-fu-save-clean" "Save Clean" "script-fu cleanup"
 "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31" "RGB* GRAY*"
 SF-IMAGE "Input Image (current image)" 0
 SF-DRAWABLE "Input Drawable (layer)" 0
 ;;SF-COLOR "Border Color" '(100 100 100)
 )

;;(script-fu-menu-register "script-fu-save-clean" "<Image>/Script-Fu")
(gimp-message "loaded gtr-save-clean.scm")
