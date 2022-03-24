;;; GIMP scheme code
(define BASE-PATH "/Users/jpeck/Google Drive/jpeckj/")

(macro (exports form)
  (let* ((syms (cdr form)))
    `(let* ((defs (map (lambda (symf)
		      (let* ((sym (if (pair? symf) (car symf) symf))
			     (binds (if (pair? symf) (cadr symf) sym))
			     (bind (eval binds)))
			`(define ,sym ,bind))) (quote ,syms))))
    `(begin ,@defs))))

(define (util-load-path0 project name)
  (let* ((base BASE-PATH)
     (gimp "GIMP/2.0")
     (proj (if (equal? project "") gimp project)))
    (string-append base proj "/scripts/" name ".scm")))

(define (util-load-path name)
  (let* ((root BASE-PATH)
	 (base "GIMP-scripts/"))
    (string-append root base name ".scm")))

;;; must use TOP-LEVEL load:
(macro (util-load-scm form)
  ;; load name from project/scripts directory
  `(load (apply util-load-path (list ,@(cdr form)))))

(define (util-file-basename filename)
  (car (reverse (strbreakup filename DIR-SEPARATOR))))

(define (util-file-extension filename)
  (car (reverse (strbreakup filename "."))))

;;; if the last thing after last 'sep' is 'suffix'
(define (string-ends-with string sep suffix)
  ;; if string ends with "<sep>suffix" and suffix does not contain <sep>
  (equal? suffix (car (reverse (strbreakup string sep)))))

(define (string-trim-end string sep)
  ;; remove 'ends-with' after sep
  (unbreakupstr (reverse (cdr (reverse (strbreakup string sep)))) sep))


(define (carloop1 thelist proc)
  ;; debug version, for-each was not giving expected result
  (while (pair? thelist)
	 (message-string "carloop1 [before] thelist:" (stringify thelist))
	 (proc (car thelist))
	 (message-string "carloop1 [after]  thelist:" (stringify thelist))
	 (set! thelist (cdr thelist))
	 ))

;; (while (pair? thelist) (proc (car thelist)) (set! thelist (cdr thelist)))
(define (carloop thelist proc)
  (for-each proc thelist))

;; (for-each proc list1 list2)
(define (carloop2 list1 list2 proc2)
  (for-each proc2 list1 list2))

;;; return first element of list satisfying pred
(define (list-search-positive list pred)
  (if (pair? list)
      (if (pred (car list)) (car list)
	  (list-search-positive (cdr list) pred))
      #f))


;; 'cont?' predicate to continue: (cont? vector index) [or to 'terminate' the loop]
;; 'proc' transform function: (proc index elt)
(define (while-vector vector cont? proc)
  (let* ((len (vector-length vector)) (index 0))
    (while (and (< index len) (if cont? (cont? vector index) #t))
	   (proc index (vector-ref vector index))
	   (set! index (+ 1 index)))))

;; do ((variable init step) ...) (test expression ...) command ...

(define (util-vector-copy vec)
  (let ((len (vector-length vec)))
    (do ((newvec (make-vector len))
	 (ndx 0 (+ ndx 1)))
	((>= ndx len) newvec)
      (vector-set! newvec ndx (vector-ref vec ndx)))))

;; or use (list->vector `(,val1 ,val2 ,val3...))
;; instead of #(val1 val2 val3 ...)
(define (util-vector-eval vec0)
  ;; only works for symbols defined in top env.
  (let ((vec (make-vector (vector-length vec0))))
    (while-vector vec0 #f (lambda(ndx val) (vector-set! vec ndx (map eval val))))
    vec))

;;; return car (or #f) and set to cdr
;; (let ((opt-arg1 (or (util-pop args) default1))
;;       (opt-arg2 (or (util-pop args) default2))
(macro (util-pop form)
  (let ((sym (cadr form)))
    `(if (pair? ,sym)
	 (let ((rv (car ,sym)))
	   (set! ,sym (cdr ,sym))
	   rv)
	 #f)))

;; (util-opt-arg sym def?)
(macro (util-opt-arg form)
  (let ((sym (cadr form))
	(def (if (pair? (cddr form)) (caddr form) nil)))
    `(or (util-pop ,sym) ,def)))

(define (with-output-to-file-catch v s p)
  ;; p has no args
  (message-string "with-output-to-file-catch" s)
  (let ((outport (open-output-file s)))
    (if (eq? outport #f)
	(begin (message-string "with-output-to-file: open-output FAILED" s) #f) ; return #f is open-output-file fails
	(let ((prev-outport (current-output-port)))
	  (set-output-port outport)
	  (let ((res (catcherr (lambda(err)(message-string "with-out-to-file-catch:" err) v) (p))))
	    (close-output-port outport)
	    (set-output-port prev-outport)
	    res)			; result is p OR v
	  ))))

;;; K-V lookup
;;; (util-assq key list [default]) -> value or [default or #f]
(macro (util-assq form)
  (let ((key (cadr form)) (lis (caddr form))
	(els (if (pair? (cdddr form)) (cadddr form) #f)))
    `(let ((v (assq ,key ,lis)))
       (if v (cadr v) ,els))))

(macro (util-assoc form)
  (let ((key (cadr form)) (lis (caddr form))
	(els (if (pair? (cdddr form)) (cadddr form) #f)))
    `(let ((v (assoc ,key ,lis)))
       (if v (cadr v) ,els))))

;;; also in: stringify.scm
(define (rgba-color? obj) 		; `(rgb r g b) OR  `(rgba r g b a)
  (and (pair? obj) 
       (or (equal? 4 (length obj)) (equal? 5 (length obj)))
       (or (equal? 'rgb (car obj)) (equal? 'rgba (car obj)))
       (number? (nth 1 obj))
       (number? (nth 2 obj))
       (number? (nth 3 obj))
       (if (= (length obj) 5) (number? (nth 4 obj)) #t))
  )

(define rgb-color? rgba-color?)

(define (rgba->string rgba)
  ;; `(rgba r g b a) ==> "rgba(r,g,b,a)" OR `(rgb r g b) ==> "rgb(r,g,b)"
  ;; ASSERT: (rgb-color? rgba)
  ;; NOTE: rgb->string method unnecessary, CSS allows "rgba(r,g,b)"
  (let ((name (symbol->string (car rgba))) ; rgb or rgba signifier
	(nums (cdr rgba)))		   ; 3 or 4 numbers (CSS doesn't really care)
    (string-append "\"" name (unbreakupstr (strbreakup (stringifyf nums) " ") ",") "\"")))

(define (color-unpack color . alpha)
  (let* ((c1 color)
	 (r (remainder c1 256))
	 (c2 (quotient c1 256))
	 (g (remainder c2 256))
	 (c3 (quotient c1 256))
	 (b (remainder c2 256))
	 (c4 (quotient c1 256))
	 (a (remainder c2 256)))
    (if (and (pair? alpha) (car alpha))
	(list r g b a)
	(list r g b))))

;;(rbg1 (color-unpack color1))
;;(rgb2 (color-unpack color2))
(define (color-dist rgb1 rgb2)
  (let ((dist 0))
    (carloop2 rgb1 rgb2
	      (lambda (c1 c2)
		(let ((d2 (* (- c1 c2) (- c1 c2))))
		  (set! dist (+ dist d2)))))
    (sqrt dist)))


;;; helper to register a menu item in GIMP/Script-Fu:
;;; inject email, name, date AND menu-register
(define (sf-reg func-name menu-item doc-string image-type . args)
  (apply script-fu-register
   func-name menu-item doc-string
   "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31"
   image-type args)
  (script-fu-menu-register func-name "<Image>/Script-Fu")
  )

;;; helper to register a menu item in GIMP/File:
(define (sf-reg-file func-name menu-item doc-string image-type . args)
  (apply script-fu-register
   func-name menu-item doc-string
   "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2021-11-11"
   image-type args)
  (script-fu-menu-register func-name "<Image>/File")
  )

(gimp-message "utils.scm loaded")
