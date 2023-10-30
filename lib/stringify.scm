;; stringify.scm
;; return a string rendition of a lisp object.
;; [better than] what the top-level REPL does, now available for use.
;;
;; suitable for gimp-message if you want to see intermediate results
;; creates a readable string, compare to "prin1"
;;
;; License: Free Open Source, copy, modify and use as you wish.
;; No warranty expressed or implied, use at your own risk.

(define (stringifier args)
  (make-environment
  (define buffer ())                    ; use cons and reverse...
  (define size 0)			; length of strings pushed to buffer
  (define max_size 2000)		; limit total size (for recursive structures)

  (define QUOTE #t)			; quote "strings" and 'symbols
  (define JSON? #f)			; KVlist->JSON { "key0" : val1 , ... }
  (define TS? #f)                       ; create alist with keyN:
  (define (display . any)) (define (newline)); do nothing in production
  (define (set-display . func) (set! display (car func)))

  (define (buffer-reset) (set! buffer ())); clear/reset buffer to empty.
  (define (buffer-add str) (set! buffer (cons str buffer))) ; 'buffer' is made of 'cons'
    
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
           
  (define (buffer-appendN . strs) (for-each buffer-append strs))

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
	  ((closure? obj)  (string->buffer "#<CLOSURE>"))
	  ((environment? obj) (string->buffer "#<ENVIRONMENT>"))
          (else            (string->buffer "???"))
          )
    )

  (define (string_nl str)
    (unbreakupstr (strbreakup str "\n") "\\n"))

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

  ;; also in utils.scm
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
    ;; assert: (or (rgb-color? rgba) (rgba-color? rgba))
    (let ((name (symbol->string (car rgba))) ; rgb or rgba signifier
	  (nums (cdr rgba)))		     ; 3 or 4 numbers (CSS doesn't really care)
      (string-append "\"" name (unbreakupstr (strbreakup (stringifyf nums) " ") ",") "\"")))


  (define (jvec->buffer obj)
    ;; left as an exercise for the motivated reader
    ;; content of vector may be any type... (obj->buffer obj "()")
    (if (< size max_size)
        (let ((ndx 0) (len (vector-length obj)))
          (buffer-append  "[ ")
	  (while (< ndx len)
	    ;; unquote is not seen inside #()
	    ;; `(#(1 2 ,three 4)) => ( [1 2 (unquote three) 4 ])
	    ;; so we detect it here, and extract the SYMBOL, and present to JSON unquoted:
	    (let* ((val0 (vector-ref obj ndx))
		   (qot (if (and (pair? val0) (eq? (car val0) 'unquote)) #f #t))
		   (val (if qot val0 (cadr val0))))
	      (jval->buffer val qot ) ; non-terminal values not supported! (maybe supply pairfunc?)
	      (set! ndx (+ 1 ndx))
	      (buffer-append (if (< ndx len) ", " ""))
	      ))
	  (buffer-append " ]")
	  )))

  (define (keyN n) (string-append "key" (number->string n)))

  (define (key? elt) (or (symbol? elt))) ;  (string? elt)
  (define (kvpair? obj) (and (pair? obj) (key? (car obj)) (= 1 (length (cdr obj)))))
  (define jval-vec #f)
  (define dual-vec #f)

  (define (jval->buffer obj QUOTE . pairfunc)
    ;; put a [right-side] key:string|symbol or [left-side] val:any
    ;; QUOTE is #f for KEY; #t for VAL
    ;; Note TS/JSON key is not quoted (JSON2 convention...)
    (set! pairfunc (if (null? pairfunc) jpair->buffer (car pairfunc)))
    (define (quote? str)
      (string->buffer (if QUOTE (string-append "\"" (string_nl str) "\"") str)))
    
    (display "jval->buffer" obj  (cond ((eq? pairfunc jpair->buffer) "jpair->buffer")
				       ((eq? pairfunc jlist->buffer) "jlist->buffer")))
    
    (cond ((null? obj)   (string->buffer "null"))
          ((symbol? obj) (string->buffer (quote? (symbol->string obj))))
	  ((string? obj) (string->buffer (quote? obj)))
          ((number? obj) (number->buffer obj))
          ((vector? obj) (jvec->buffer obj))
	  ((equal? obj #t) (obj->buffer 'true "true"))
	  ((equal? obj #f) (obj->buffer 'false "false"))
	  ((rgba-color? obj) (string->buffer (rgba->string obj)))
          ((pair?  obj)      (pairfunc obj 0))
	  (else
	   (display "jval->buffer: else" (stringify obj) )(newline)
	   (string->buffer "???"))
          ))

  ;; default pairfunc to format list/pair (key . val) as:    {keyN: [key, ...vals]} OR {sym: ,val} OR {sym: val}
  ;; (key . val) ==> {key: [...val]}  or [key, ...val]  (OR { keyN: [key, ...vals] }
  (define (jpair->buffer obj n)		; obj is (list car cadr)
    (if (< size max_size)
        (let* ((key (car obj)) (val (cdr obj)) (len (length val)))
	  (display (stringifyf "jpair->buffer: key=" key ", val=" val ", len=" len ", n=" n)) (newline)
	  (buffer-append (if (= n 0) "" ", ")) ; adjacent to initial "{"
	  (buffer-append "{") (display (buffer->string)) (newline)
	  (if (not (symbol? key))
	      (begin			   ; strange case...
		(display (stringifyf "jpair->buffer.strange case: n=" n ", obj=" obj)) (newline)
		(jval->buffer (keyN n) #f) ; nullstr not used
		(buffer-append " : ")	   ;
		(jval->buffer (list->vector obj) #t)) ; pair is not a kvpair ==> send list as vector (jval-vec)!
	    (begin			   ;
	      (display (stringifyf "jpair->buffer.normal: key=" key ", val=" val "len=" len)) (newline)
	      (jval->buffer key #f)	   ; key should never be null?: symbol (OR string)
	      (buffer-append ": ")	   ;
	      (if (= (length val) 1)	   ; key : val  OR key : {  }
		  (jval->buffer (car val) #t) ; QUOTE ???
		(jval->buffer val #t)
		)))
	  (buffer-append "}") (display "}") (newline))
      ))

  ;; pairfunc to ouput as vector: ((a 1) b (c any) [] ) ==> [{a: 1}, b, {c: any}, []]
  (define (json-vec pair n)			; [{a: 1}, b, {c: 2}, {d: [...]} ]
    (buffer-append (if (= n 0) "" ", ")) ; adjacent to initial "{"
    (jval->buffer (list->vector pair) #t)
  )

  (define (kvlist? obj)
    (define (not-kvp? elt) (not (and (pair? elt) (symbol? (car elt)) (< (length (cdr elt)) 2))))
    ;; TODO: confirm each key is unique!
    (and (pair? obj) (not (find not-kvp? obj))) ; true if all elt of obj are kvp?
    )


  ;; pairfunc to format list/pair as an Object: {k0: v0, k1: v1, ...}
  ;; suitable for '((a 1) (b 2) ... (z 30)) ==> {a: 1, b: 2, ... z: 30}   simple/ideal case.
  ;; ((a 1) b (c 2) (d [x1 x2 x2]) ) ==> {a: 1, key1: b, d: [...])  OR [{a: 1}, b, {c: 2}, {d: [...]} ]
  ;; ( a b c d) ==> [a, b, c, d] OR {key1: a, key2: b, key3: c, key4: d}
  ;; ((a 1) ("foo" 3)) ==> {a: 1, key1: ["foo", 3] }


  (define (jlist->buffer obj n)
    (display "jlist->buffer:" (stringify obj)) (newline)
    (let* ((nul "undefined"))

      ;; add k:v pair to current object: { ... , k: v, ...}
      (define (emit-inline elt)		; ambient (n)
	(display "elt: car=" (stringify (car elt)) "  cdr=" (stringify (cdr elt)) "\n")
	(buffer-append (if (= n 0) "" ", "))
	(jval->buffer (car elt) #f) ; key (symbol|string) is not quoted, ASSERT: pairfunc not used
	(buffer-append ": ")	    ;
	(jval->buffer (cadr elt) #t jlist->buffer)) ; quote string, symbol-name, recurse to here for lists

      (define (elt->buffer elt)
	(display (string-append "elt->buffer: elt=" (stringify elt))) (newline)
	(cond
	 ((null? elt))
	 ((pair? elt)			; kvpair? (sym val) or unnamed obj? ((..) ...)
	  (let ((len (length elt)))	; no dotted pair allowed...
	    (cond
	     ((= len 0) (emit-inline (list (keyN n) nul))) ; ((kv) (kv) () (kv))
	     ((= len 1) (emit-inline (cons (keyN n) elt))) ; (list (keyN n) (car elt)), kn:"val"
	     ((kvpair? elt) (emit-inline elt))		   ; (k val)   => , k: val
	     ((key? (car elt))				   ; (k (...)) => , k: {...}
	      (emit-inline (list (car elt) (cdr elt))))	   ; the Trick! coerce to named-list ['list' not 'cons']
	     (else			; malformed KV element! (complex key) kn: {...}
	      (emit-inline (list (keyN n) elt)))
	     )))
	 (else (emit-inline (list (keyN n) elt)))) ; kn: { }
	(set! n (+ 1 n)))


      (buffer-append "{")
      (cond
       ((kvpair? obj) (emit-inline obj))		    ; (k v) (elt->buffer obj)
       ((key? (car obj))				    ; (k (...)) => , k: {...} (elt->buffer obj)
	(emit-inline (list (car obj) (list->vector (cdr obj))))) ; (k: [...])
       (else (for-each elt->buffer obj)))		    ; actual list of: ((K V)(K V)...)
      (buffer-append "}")
      ))


  (define (set-args args)
    ;; Parse given args: obj [max-len] '% 'JSON 'TS
    ;; arg1: obj
    ;; arg2: [buffer-len]  <--- (number? arg2)
    ;; arg3: '% 'JSON 'TS
    ;; '% ==> (QUOTE #f)
    ;; 'JSON ==> alist/pairs: ((caar1 cdar1) (caar2 cdar2) ... ) {caar1: cdar1, caar2: cdar2, ...}
    ;; 'TS ==>  (car1 car2 (caar3 cdar3) ...)  {key1: car1, key2: car2, caar3: cdar3, ...}
    ;; 'D  ==> log display to gimp-message
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
    (when (and (pair? args) (eq? (car args) 'D))
      (set-display message-string)
      (set! args (cdr args)))
    )
  (set-args args)
 
  (define (stringify0 obj . args)
    (set-args args) 			; set TS? JSON? QUOTE
    (buffer-reset)
    (cond
     (TS?    (jval->buffer obj #t jlist->buffer)) ; QUOTE=#f, TS?  =t
     (JSON?  (jval->buffer obj #t jlist->buffer)) ; QUOTE=#f, JSON?=t
     (QUOTE  (obj->buffer obj "()"))		   ; QUOTE=#t, GIMP/TinyScheme form of nil
     (else   (tail->buffer obj)))			 ; 
    (buffer->string))

  ) ; make-env
  ) ; stringifier

(define (stringify obj . args)		; returns String
  (let ((fier (stringifier args)))
    (fier::stringify0 obj)
    ))

(define (stringifyf . args) (stringify args '%))
  
(define (message-string0 . args)
  ;; unquote all
  (gimp-message (stringify args)))

(define (message-string . args)
  ;; unquote all
  (gimp-message (stringify args '%)))

(define (message-string1 loc . args)
  ;; unquotify first quote rest: first (,@rest)
  (gimp-message (string-append (stringifyf loc) (stringify args))))

(define (message-string2 . args)
  ;; quotify all
  (gimp-message (stringify args)))

;; (stringify-test 2) runs (stringify obj2)
;; args can be ... but default includes 'TS to translate TypeScript vs 'JSON
(define (stringify-test val . args)
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
	 (obj8 `(p #(1 2 3 "foo" "bar"))) ; test VECTOR
	 (xxx (if (number? val) (set! val (nth val (list obj0 obj1 obj2 obj3 obj4 obj5 obj6 obj7 obj8)))))
	 (val2 (if (null? val) obj0 val))
	 (flags `(,@args TS))
	 (d1 (display (string-append "  val2=" (stringify val2))))
	 (d2 (display (string-append "  flags=" (stringify flags))))
	 (d3 (newline))
	 (rv (apply stringify val2 flags)))
    (display val2)
    (newline)
    (display rv)))

(define fier (stringifier ()))


(gimp-message "stringify.scm loaded")
