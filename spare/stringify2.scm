;; stringify.scm
;; return a string rendition of a lisp object.
;; [better than] what the top-level REPL does, now available for use.
;;
;; suitable for gimp-message if you want to see intermediate results
;; creates a readable string, compare to "prin1"
;;
;; License: Free Open Source, copy, modify and use as you wish.
;; No warranty expressed or implied, use at your own risk.

;;(define (stringify obj . args)		; returns String
  (define max_size 2000)
  (define buffer ())                    ;use cons and reverse...
  (define size 0)
  (define QUOTE #t)			;quote "strings" and 'symbols
  (define JSON? #f)			;KVlist->JSON { "key0" : val1 , ... }
  ;;(define (display . any)) (define (newline)); do nothing in production
  (define (buffer-add str) (set! buffer (cons str buffer)))
    
  (define (buffer->string)
    (if (>= size max_size)
        (buffer-add "..."))
    (let* ((buf (reverse buffer)))
      (apply string-append buf)))


  (define (buffer-append str)
    (if (string? str) ; when full, pair->buffer may return nil;
        (begin
          (set! size (+ size (string-length str)))
          (if (< size max_size)
              (buffer-add str)
              ))))
           
  (define (obj->buffer obj nullstr)	;return string or buffer
    ;; NEW: always push to buffer, ignore return value.
    (cond ((null? obj) (string->buffer nullstr))
          ((symbol? obj) (string->buffer 
			  (if QUOTE (string-append "'" (symbol->string obj))
			      (symbol->string obj))))
	  ((string? obj) (string->buffer
			  (if QUOTE (string-append "\"" obj "\"")
			      obj)))
          ((number? obj) (number->buffer obj))
          ((vector? obj) (vector->buffer obj))
          ((pair?   obj) (pair->buffer obj))
	  ((equal? obj #t) (string->buffer "#t"))
	  ((equal? obj #f) (string->buffer "#f"))
          (else            (string->buffer "???"))
          )
    )

  (define (string->buffer str)
    (if (< size max_size)
	(buffer-append str)))

  (define (number->buffer num)
    (if (< size max_size)
	(buffer-append (number->string num))))

  (define (pair->buffer obj) ; buffer or #f
    (if (< size max_size)
        (begin
          (buffer-append "(")
          (obj->buffer (car obj) "nil") ; or "()"
          (tail->buffer (cdr obj))
	  (buffer-append ")")
          )
        )
    )

  (define (tail->buffer obj) ;; return void
    (if (< size max_size)
        (begin
          (cond
           ((pair? obj)
            (buffer-append (if (= size 0) "" " ")) ; nothing to tail yet
            (obj->buffer (car obj) "nil" )
            (tail->buffer (cdr obj))
            )
           ((null? obj))
           (else
            (buffer-append " . ")
            (obj->buffer obj "")
            ))
          )
        )
    )

  (define (vector->buffer obj)
    ;; left as an exercise for the motivated reader
    ;; content of vector may be any type... (obj->buffer obj "()")
    (if (< size max_size)
        (let ((ndx 0) (len (vector-length obj)))
          (buffer-append  "#( ")
	  (while (< ndx len)
		 (obj->buffer (vector-ref obj ndx) "()")
		 (set! ndx (+ 1 ndx))
		 (buffer-append (if (< ndx len) " " ""))
		 )
	  (buffer-append " )")
	  )))

  (define (rgb-color? obj)
    (and (pair? obj)
	 (equal? 3 (length obj))
	 (number? (nth 0 obj))
	 (number? (nth 1 obj))
	 (number? (nth 2 obj))))

  (define (rgba-color? obj)
    (and (pair? obj)
	 (equal? 4 (length obj))
	 (number? (nth 0 obj))
	 (number? (nth 1 obj))
	 (number? (nth 2 obj))
	 (number? (nth 3 obj))))

  (define (rgba->string rgba)
    ;; rgba ISA rgb-color? or rgba-color?
    (and (eq? 3 (length rgba))(set! rgba (append rgba `(1))))
    (string-append "\"rgba" (unbreakupstr (strbreakup (stringifyf rgba) " ") ",") "\""))


  (define (jvec->buffer obj)
    ;; left as an exercise for the motivated reader
    ;; content of vector may be any type... (obj->buffer obj "()")
    (if (< size max_size)
        (let ((ndx 0) (len (vector-length obj)))
          (buffer-append  "[ ")
	  (while (< ndx len)
		 (jval->buffer (vector-ref obj ndx) #t ) ; non-terminal values not supported! (pairfunc)
		 (set! ndx (+ 1 ndx))
		 (buffer-append (if (< ndx len) ", " ""))
		 )
	  (buffer-append " ]")
	  )))

  (define (jtail->buffer obj n) ;; return buffer or #f
    ;; put series of "nth" jpair->buffer, creating keyN as necessary
    ;;(display (stringifyf "jtail->buffer:" obj n " "))
    (if (< size max_size) ; or use (while ...) to iterate vs recurse
        (begin
          (cond
           ((pair? obj)
	    (let* ((first (car obj)) (rest (cdr obj)))
	      (display (stringifyf "first =" first "  rest="rest "\n"))
	      (if (pair? first)
		  (begin		    ; (xx (a 1) (b 2))  vs (x 1)
		    (jpair->buffer first n) ; although: if (pair? (cadr first)) then ...?
		    (jtail->buffer rest (+ 1 n))
		    )
		  (if (> (length rest) 0)
		      (jpair->buffer (cons first (list rest)) n)
		      (jpair->buffer (list (keyN n) first) n))
		  )
	      ))
	   ((null? obj)
	    ;;(display "\n")
	    )
           (else			;(... . X) --> (... . (list keyN X))
            (jpair->buffer (list (keyN n) obj) n)
            ))
	  )))

  (define (jpair->buffer obj n)		; obj is (list car cadr)
    (if (< size max_size)
        (let* ((key (car obj)) (val (cdr obj)) (len (length val)))
	  (display (stringifyf "jpair(" key " : "  val ")  len=" len "\n"))
	  (buffer-append (if (= n 0) "" ", ")) ; adjacent to initial "{"
	  (if (pair? key)
	      (begin
		(jval->buffer (keyN n) #f) ; nullstr not used
		(buffer-append " : ")	   ;
		(jval->buffer obj #t))	   ; nullstr not used
	      (begin			   ;
		(jval->buffer key #f)	   ; key should never be null?: string OR symbol
		(buffer-append ": ")	   ;
		(if (= (length val) 1)	   ; key : val  OR key : {  }
		    (jval->buffer (car val) #t)
		    (jval->buffer val #t)
		    )))
          )))

  (define (jval->buffer obj QUOTE . pairfunc)
    ;; put a [right-side] key:string|symbol or [left-side] val:any
    ;; QUOTE is #f for KEY; #t for VAL
    ;; Note TS/JSON key is not quoted
    (set! pairfunc (if (null? pairfunc) jtail->buffer (car pairfunc)))
    (define (quote? str)
      (string->buffer (if QUOTE (string-append "\"" str "\"") str)))
      
    (cond ((null? obj)   (string->buffer "null"))
          ((symbol? obj) (string->buffer (quote? (symbol->string obj))))
	  ((string? obj) (string->buffer (quote? obj)))
          ((number? obj) (number->buffer obj))
          ((vector? obj) (jvec->buffer obj))
	  ((equal? obj #t) (string->buffer "true"))
	  ((equal? obj #f) (string->buffer "false"))
	  ((rgb-color? obj) (string->buffer (rgba->string obj)))
	  ((rgba-color? obj) (string->buffer (rgba->string obj)))
          ((pair?  obj)      (buffer-append "{")
	   (display "{")(newline)
	   (pairfunc obj 0)
	   (display "}")(newline)
	   (buffer-append "}")) ; right-side Object
          (else              (string->buffer "???"))
          )
    )
  (define (keyN n) (string-append "key" (number->string n)))

  (define (kvlist->buffer obj n)
    (display (string-append "kvlist:" (stringify obj) "\n"))
    (let* ((nul "undefined") (kvlist kvlist->buffer))
      (define (key? elt) (or (symbol? elt) (string? elt)))
      (define (kvpair? pair) (and (key? (car pair)) (= 2 (length pair))))
      (define (kvpair->buffer kvpair)
	(display (string-append "kvpair: car=" (stringify (car kvpair)) "  cdr=" (stringify (cdr kvpair)) "\n"))
	(buffer-append (if (= n 0) "" ", "))
	(jval->buffer (car kvpair) #f kvlist)  ; key is not quoted, ASSERT: pairfunc not used
	(buffer-append ": ")		       ;
	(jval->buffer (cadr kvpair) #t kvlist) ; quote string, symbol-name, back here for lists
	)
      (define (elt->buffer elt)
	(display (string-append "elt->buffer: elt=" (stringify elt) "\n"))
	(cond
	 ((null? elt))
	 ;;((and (pair? elt) (null? (cdr elt))) ; dotted pair!? ((x (...))) ; ((x (...)) . ())
	 ;; (jpair->buffer (list (car elt) (cdr elt)) n))
	 ((pair? elt)			; kvpair? (sym val) or unnamed obj? ((..) ...)
	  (let ((len (length elt)))	; no dotted pair allowed...
	    (cond
	     ((= len 0) (kvpair->buffer (list (keyN n) nul))) ; ((kv) (kv) () (kv))
	     ((= len 1) (kvpair->buffer (cons (keyN n) elt))) ; (list (keyN n) (car elt)), kn:"val"
	     ((kvpair? elt) (kvpair->buffer elt))	      ; (k val)   => , k: val
	     ((key? (car elt))				      ; (k (...)) => , k: {...}
	      (kvpair->buffer (list (car elt) (cdr elt))))    ; the Trick! coerce to named-list ['list' not 'cons']
	     (else			; malformed KV element! (complex key) kn: {...}
	      (kvpair->buffer (list (keyN n) elt)))
	     )))
	 (else (kvpair->buffer (list (keyN n) elt)))) ; kn: { }
	(set! n (+ 1 n)))

      (cond
       ((kvpair? obj) (kvpair->buffer obj))		    ; (k v) (elt->buffer obj)
       ((key? (car obj))				    ; (k (...)) => , k: {...} (elt->buffer obj)
	(kvpair->buffer (list (car obj) (cdr obj)))) ; 
       (else (for-each elt->buffer obj)))		    ; actual list of: ((K V)(K V)...)
      ))

  ;; (stringify obj . args)

  ;; arg1: obj
  ;; arg2: [buffer-len]
  ;; arg3: '% 'JSON 'TS
  ;; '% ==> (QUOTE #f)
  ;; 'JSON ==> alist/pairs: ((caar1 cdar1) (caar2 cdar2) ... ) {caar1: cdar1, caar2: cdar2, ...}
  ;; 'TS ==>  (car1 car2 (caar3 cdar3) ...)  {key1: car1, key2: car2, caar3: cdar3, ...}

(define (stringify2 obj . args)		; returns String
  ;; due to breaking the closure, need to reset these:
  (set! max_size 400)
  (set! buffer ())
  (set! size 0)
  (set! QUOTE #t)
  (set! JSON? #f)
  ;;(display (string-append "stringify2: args = " (stringify args) "\n"))
  ;;(message-string "stringify2: args = " args)
  (define TS? #f)
  (when (and (pair? args) (number? (car args)))
	(set! max_size (car args))
	(set! args (cdr args)))
  (when (and (pair? args) (eq? (car args) '%))
	(set! QUOTE #f)
	(set! args (cdr args)))
  (when (and (pair? args) (eq? (car args) 'JSON))
	(set! JSON? #t)		; process as alist
	(set! QUOTE #f)
	(set! args (cdr args)))
  (when (and (pair? args) (eq? (car args) 'TS))
	(set! TS? #t)		; create alist with keyN:
	(set! QUOTE #f)
	(set! args (cdr args)))
  (cond
   (TS?    (jval->buffer obj #t kvlist->buffer))
   (JSON?  (jval->buffer obj #t kvlist->buffer)) ; (jval->buffer obj #t))
   (QUOTE  (obj->buffer obj "()"))
   (else   (tail->buffer obj)))

  (buffer->string)
  )



(define (stringify-test2 val . args)
  ;;(define (display . any)) (define (newline))
  (let* ((obj0 `((p ((q ((d 4)    ((k 1) (m 2))  (b bval))) (color2 (180 255 50 99)) (t #t) (f #f)) (k kval))))
	 ;; obj0: outer= { key0, k}, key0:{ k0: "p", q, color t, f}
	 (obj1  `(p ((q ((d 4)    ((k 1) (m 2))  (b bval))) (color2 (180 255 50 99)) (t #t) (f #f)) (k kval)))
	 (obj2  `(p  (q ((d 4) (s ((k 1) (m 2))) (b bval))) (color2 (180 255 50 99)) (t #t) (f #f) (k kval)))
	 (obj3  `(p  (q ((d 4)    ((k 1) (m 2))  (b bval))) (color2 (180 255 50 99)) (t #t) (f #f) (k kval)))
	 (obj4 `(p ( (q ((d 4)    ((k 1) (m 2))  (b bval))) (color2 (180 255 50 99)) (t #t) (f #f) (k kval))))
	 (obj5 `((p ((q ((d 4)    ((k 1) (m 2))  (b bval))) (color2 (180 255 50 99)) (t #t) (f #f) (k kval)))))
	 ;; obj5: p named, with {q,b,color2, t,f,k}
	 (obj6 `((p ((q ((d 4)    ((k 1) (m 2))  (b bval))) (color2 (180 255 50 99)) (t #t) (f #f))) (k kval) ))
	 ;; obj6: p named, with 2 elts
	 ;; {p: {q: {d: 4, key1: {k: 1, m: 2}, b: "bval"}, color2: "rgba(180,255,50,99)", t: true, f: false}, k: "kval"}
	 (obj7 `((p ((q ((d 4) (s ((k 1) (m 2))) (b bval))) (color2 (180 255 50 99)) (t #t) (f #f))) (k kval) ))
	 ;; obj7: outer has 2 elts, p named with 2 elts, q inner named s:{}
	 ;; {p: {q: {d: 4, s: {k: 1, m: 2}, b: "bval"}, color2: "rgba(180,255,50,99)", t: true, f: false}, k: "kval"}
	 (xxx (if (number? val) (set! val (nth val (list obj0 obj1 obj2 obj3 obj4 obj5 obj6 obj7)))))
	 (val2 (if (null? val) obj0 val))
	 (flags `(,@args TS))
	 (d1 (display (string-append "  val2=" (stringify val2))))
	 (d2 (display (string-append "  flags=" (stringify flags))))
	 (d3 (newline))
	 (rv (apply stringify2 val2 flags)))
    (display val2)
    (newline)
    (display rv)))

(gimp-message "stringify.scm loaded")
