;; kumbbl-string-filename-utilities.scm
;; by Klaus Berndl

;; Description
;;
;; This implements a some utilitis for string- and filename-handling
;; exports - see funtions below and the comments in front of them

;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version. 
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; The GNU Public License is available at
;; http://www.gnu.org/copyleft/gpl.html

;; ---------- string-utilities -----------------

;; Return the index of the first occurence of a-char in str, or #f
(define (string-index str a-char)
  (let loop ((pos 0))
    (cond
     ((>= pos (string-length str)) #f) ; whole string has been searched, in vain
     ((char=? a-char (string-ref str pos)) pos)
     (else (loop (+ 1 pos))))))

;; Return the index of the last occurence of a-char in str, or #f
(define (string-index-right str a-char)
  (let loop ((pos (- (string-length str) 1)))
    (cond
     ((negative? pos) #f) ; whole string has been searched, in vain
     ((char=? a-char (string-ref str pos)) pos)
     (else (loop (- pos 1))))))
(define string-rindex string-index-right)


;; string-contains s1 s2 [start1 end1 start2 end2] -> integer or false
;; string-contains-ci s1 s2 [start1 end1 start2 end2] -> integer or false
;; Does string s1 contain string s2?
;; Return the index in s1 where s2 occurs as a substring, or false. The
;; optional start/end indices restrict the operation to the indicated
;; substrings.
;; We do not support the optional arguments
(define (string-contains str pattern)
  (let* ((pat-len (string-length pattern))
	 (search-span (- (string-length str) pat-len))
	 (c1 (if (zero? pat-len) #f (string-ref pattern 0)))
	 (c2 (if (<= pat-len 1) #f (string-ref pattern 1))))
    (cond
     ((not c1) 0) ; empty pattern, matches upfront
     ((not c2) (string-index str c1)) ; one-char pattern
     (else ; matching a pattern of at least two chars
      (let outer ((pos 0))
	(cond
	 ((> pos search-span) #f) ; nothing was found thru the whole str
	 ((not (char=? c1 (string-ref str pos)))
	  (outer (+ 1 pos))) ; keep looking for the right beginning
	 ((not (char=? c2 (string-ref str (+ 1 pos))))
	  (outer (+ 1 pos))) ; could've done pos+2 if c1 == c2....
	 (else		     ; two char matched: high probability
			     ; the rest will match too
	  (let inner ((i-pat 2) (i-str (+ 2 pos)))
	    (if (>= i-pat pat-len) pos ; whole pattern matched
		(if (char=? (string-ref pattern i-pat)
			    (string-ref str i-str))
		    (inner (+ 1 i-pat) (+ 1 i-str))
		    (outer (+ 1 pos)))))))))))) ; mismatch after partial match

(define (substring? pattern str) (string-contains str pattern))


;; Here are some specialized substring? functions
;; checks to make sure that PATTERN is a prefix of STRING
;;
;; (string-prefix? "pir" "pirate") => #t
;; (string-prefix? "rat" "outrage") => #f
;; (string-prefix? "" any-string) => #t
;; (string-prefix? any-string any-string) => #t
(define (string-prefix? pattern str)
  (let loop ((i 0))
    (cond
     ((>= i (string-length pattern)) #t)
     ((>= i (string-length str)) #f)
     ((char=? (string-ref pattern i) (string-ref str i))
      (loop (+ 1 i)))
     (else #f))))

(define (string-prefix-ci? pattern str)
  (let loop ((i 0))
    (cond
     ((>= i (string-length pattern)) #t)
     ((>= i (string-length str)) #f)
     ((char-ci=? (string-ref pattern i) (string-ref str i))
      (loop (+ 1 i)))
     (else #f))))

;; checks to make sure that PATTERN is a suffix of STRING
;;
;; (string-suffix? "ate" "pirate") => #t
;; (string-suffix? "rag" "outrage") => #f
;; (string-suffix? "" any-string) => #t
;; (string-suffix? any-string any-string) => #t
(define (string-suffix? pattern str)
  (let loop ((i (dec (string-length pattern))) (j (dec (string-length str))))
    (cond
     ((negative? i) #t)
     ((negative? j) #f)
     ((char=? (string-ref pattern i) (string-ref str j))
      (loop (dec i) (dec j)))
     (else #f))))

(define (string-suffix-ci? pattern str)
  (let loop ((i (dec (string-length pattern))) (j (dec (string-length str))))
    (cond
     ((negative? i) #t)
     ((negative? j) #f)
     ((char-ci=? (string-ref pattern i) (string-ref str j))
      (loop (dec i) (dec j)))
     (else #f)))) 

(gimp-message "strings.scm loaded")
