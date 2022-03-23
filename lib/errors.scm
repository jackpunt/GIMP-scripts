;;;; Injected into script-fu.init

;;;; Simple exception handling
;
;    Exceptions are caught as follows:
;
;         (catch (do-something to-recover and-return meaningful-value)
;              (if-something goes-wrong)
;              (with-these calls))
;
;    "Catch" establishes a scope spanning multiple call-frames
;    until another "catch" is encountered.
;
;    Exceptions are thrown with:
;
;         (throw "message")
;
;    If used outside a (catch ...), reverts to (error "message)

;; (define *handlers* (list))

;; (define (push-handler proc)
;;   (set! *handlers* (cons proc *handlers*)))

;; (define (pop-handler)
;;   (let ((h (car *handlers*)))
;;     (set! *handlers* (cdr *handlers*))
;;     h))

;; (define (more-handlers?)
;;      (pair? *handlers*))

;; (define (throw . x)
;;   (if (more-handlers?)
;;       (apply (pop-handler) x)
;;       (apply error x)))

;; (macro (catch form)
;;   (let* ((xarg (gensym))
;; 	 (exit (gensym))
;; 	 (label (gensym))
;; 	 )
;;     `(call/cc (lambda (,exit)
;; 		(push-handler (lambda (,xarg)
;; 				(gimp-message ,xarg)
;; 				(,exit ,(cadr form))))
;; 		(let ((,label (begin ,@(cddr form))))
;; 		  (pop-handler)
;; 		  ,label)))))


;; (macro (catcherr form)
;;   (let* ((xarg (gensym))
;; 	 (exit (gensym))
;; 	 (label (gensym))
;; 	 (xvalf `(,(cadr form) ,xarg))
;; 	 )
;;     `(call/cc (lambda (,exit)
;; 		(push-handler (lambda (,xarg)
;; 				(,exit ,xvalf)))
;; 		(let ((,label (begin ,@(cddr form))))
;; 		  (pop-handler)
;; 		  ,label)))))

;; (define *error-hook* throw)
