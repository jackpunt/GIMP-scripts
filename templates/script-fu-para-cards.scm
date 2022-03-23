;;;
;;; automate production of publish images and pngs
;;
;;; open the .xcf for the publish sheets
;;; parasites identify which card is in which slot
;;; card-to-template checks to see where that card goes and [re]-writes to those slots

(define PARA-PREFIX "card-in:")

;;; parasites defined:
;;; pname is location of card: `card-in:I:L:X:Y` --> image-id: number
;;; para-set-comment [gimp-comment], para-set-display [card-display], para-set-pubname, para-set-global-pubname


;;; Advice in card-to-this-ilxy
(define (card-parasite-record-image image destImage destLayer destX destY)
  ;; record association of srcImage with Dest (I L X Y)
  (let ((pname (card-in-ilxy destImage destLayer destX destY)) ; "card-in:I:L:X:Y"
	(image-id (ident-of image))	; "cardname.png:image"
	(dest-id (ident-of destImage)))	; template-N.xcf (TODO: rename when save template)
    (gimp-image-attach-parasite destImage      (list pname 1 image-id))
    (gimp-image-attach-parasite image          (list pname 1 dest-id))
    ;;(gimp-attach-parasite (list (new-global-name image-id) 1 dest-id))
    ))


(define (process-card-images destImage proc)
  ;; for each "card-in:" parasite of destImage, process to open or close:
  (let* ((layers (gimp-image-get-layers destImage))
	 (length (car layers))
	 (destLayer (vector-ref (cadr layers) (- length 1)))
	 (dest-id (ident-of destImage))
	 ;; capture all old pnames before removing
	 (pnames (cadr (gimp-image-get-parasite-list destImage))))
    (carloop pnames (lambda (pname)
		      ;;(message-string "process-card-images:" pname PARA-PREFIX)
		      (if (starts-with? pname PARA-PREFIX)
			  (let ((card-id (nth 2 (car (gimp-image-get-parasite destImage pname))))) ; ASSERT pname is valid parasite
			    ;;(message-string "pci-1: pname=" pname card-id)
			    (proc card-id pname destImage destLayer)))))
    ))

(define (para-remove-dest-id card-id pname cardImage baseLayer)
  ;; remove dest-id from card-image
  ;; assert (starts-with? pname PARA-PREFIX)
  ;;(message-string "para-remove-dest-id" card-id pname cardImage)
  (gimp-image-detach-parasite cardImage pname))

(define (sf-open-card-images destImage)
  ;; open all the cards that tiled onto destImage [template]
  
  ;; very likely, this template has changed its name (and certainly 'image' and 'layer')
  ;; the good bits are the destX, destY and the card-name.png (stored in parasites "card-id:...")
  ;; (open-card-image card-id pname destImage destLayer)
  (process-card-images destImage open-card-image))
	   
(define (sf-close-card-images destImage)
  ;; (close-card-image card-id pname destImage destLayer)
  (process-card-images destImage close-card-image))  

(define (para-set-display image display)
  ;; assert: (number? display)
  (message-string "para-set-display" image display)
  (gimp-image-attach-parasite image (list "card-display" 1 (number->string display)))
  display)

(define (para-get-display image)
  ;; check result using (gimp-display-is-valid)
  (catch (lambda(err) (message-string "para-get-display" image err) #f)
	 (string->number (nth 2 (car (gimp-image-get-parasite image "card-display"))))))

(define (para-set-global-pubname pubname)
  (gimp-attach-parasite (list "card-global-pubname" 1 pubname)) pubname)

(define (para-get-global-pubname)
  (catch #f (nth 2 (car (gimp-get-parasite "card-global-pubname")))))

(define (para-unset-global-pubname)
  ;; provoke -get- to return #f
  (gimp-detach-parasite "card-global-pubname"))

(define (para-set-pubname image pubname)
  ;; assert: (string? pubname)
  (gimp-image-attach-parasite image (list "card-pubname" 1 pubname)) pubname)

(define (para-get-pubname image)
  (catch (lambda(err) (message-string "para-get-pubname" image err) #f)
	 (nth 2 (car (gimp-image-get-parasite image "card-pubname")))))


;;; card-id = "card-name.png"
(define (open-card-image card-id pname destImage destLayer) 
  ;; find or open card-image associated to destImage bye card-id
  ;;(message-string "open-card-image" card-id pname)
  (let* ((origILXY (parse-card-in pname)) ; (oldI oldL destX destY)
	 (destX (nth 2 origILXY))
	 (destY (nth 3 origILXY))
	 ;;
	 (card-name (car (strbreakup card-id ":"))) ; card-name.png : N
	 (cardimage (card-image card-name)))
    ;;(message-string "open-card-image found cardimage=" cardimage)
    (if (null? cardimage)
	(let* ((filename (string-append CARD-DIR card-name)))
	  ;;(message-string "open-card-image open:" filename card-name)
	  (set! cardimage (car (gimp-file-load RUN-NONINTERACTIVE filename card-name)))))
    (or (para-get-display cardimage)
	(para-set-display cardimage (car (gimp-display-new cardimage))))
    ;; detach old info
    (gimp-image-detach-parasite destImage pname)
    (process-card-images cardimage para-remove-dest-id)
    ;; attach new info
    (card-parasite-record-image cardimage destImage destLayer destX destY)))

(define (para-kill-display image)
  ;; kill display of image if recorded and [still] exists:
  (let ((display (para-get-display image)))
    (if (and display (= TRUE (car (gimp-display-is-valid display))))
	(gimp-display-delete display))
    ))

(define (close-card-image card-id pname destImage destLayer)
  ;; find referenced image from template/card-id
  ;; save file to image-filename and remove display for destImage
  ;;(message-string "close-card-image" card-id "pname=" pname "destImage=" destImage)
  (let* ((cardname-image (strbreakup card-id ":")); (card-name.png image)
	 (cardname (nth 0 cardname-image))	  ; basename (= title-dash.png)
	 (imagestr (nth 1 cardname-image))
	 (image (string->number imagestr))
	 (filename0 (string-append CARD-DIR cardname)))
    (when (gimp-image-is-valid image)  ; image maybe not open
	  ;; (catch nil ; because display->image may change async...
	  (file-merge-and-save image cardname)
	  (para-kill-display image)
	  (gimp-displays-flush)
	  )
    
    ))
(define (para-set-comment image comment)
  ;;(message-string "para-set-comment" image comment)
  (gimp-image-attach-parasite image (list "gimp-comment" 1 comment)))

;;; This may even work for basename.xcf
(define (file-merge-and-save image . cardname)
  ;; filename set by open-all OR by card-set-title (deck-builder)
  ;; expect card.png to be in CARD-DIR
  ;; template.xcf in TEMPLATE-DIR (and template.png in PUB-DIR)
  (let* ((filename (car (gimp-image-get-filename image))) ; as was loaded (pro'ly CARD-DIR)
	 (toplayer (car (gimp-image-merge-visible-layers image CLIP-TO-IMAGE)))
	 (basename (util-file-basename filename)))
    ;;(message-string "file-merge-and-save0" cardname basename filename)
    (set! cardname (if (pair? cardname) (car cardname) basename))
    ;;(message-string "file-merge-and-save1" cardname basename filename)
    (para-set-comment image cardname)
    ;;(message-string "file-merge-and-save2" cardname filename toplayer)
    (gimp-file-save RUN-NONINTERACTIVE image toplayer filename cardname)
    ;;(message-string "file-merge-and-save3" cardname filename toplayer)
    ))
  

(define (card-image cardname)
  ;; look for an open image with basename(image) = cardname
  ;; maybe check ".../cards/basename.png"
  (let* ((images (gimp-image-list))
	 (imagen (car images))
	 (imagev (cadr images))
	 (index 0)
	 (cardimage nil))
    ;; terminating loop:
    (while (and (null? cardimage) (< index imagen))
	   (let* ((image (vector-ref imagev index))
		  (filename (car (gimp-image-get-filename image)))
		  (basename (car (reverse (strbreakup filename "/")))))
	     (if (string=? basename cardname) (set! cardimage image)))
	   (set! index (+ 1 index)))
    cardimage))

;;; TODO: when loading pub.xcf, find the card-to: parasites, and push to global parasites
;;; TODO: when loading card.png, find pub-ilxys and push to card.xcf
;;; OR! load template.xcf, use that to OPEN the cards/files.png (and set the parasites)

(define (templates-update-card image drawable)
  ;; for each template holding 'image' replace with current/edited version 
  (let ((undo #t)
	(ilxys (pub-ilxys image)))
    (and undo (gimp-image-undo-group-start image)) ; mark for undo
    (carloop ilxys (lambda (ilxy)
		     (message-string "card-to:" image ilxy)
		     (card-to-this-ilxy image ilxy #t)))
    (and undo (gimp-image-undo-group-end image))
    ))

(define (templates-replace-image image drawable)
  ;; for each template holding 'image' replace with current/edited version 
  (let ((undo #t)
	(ilxys (pub-ilxys image)))
    (and undo (gimp-image-undo-group-start image)) ; mark for undo
    (carloop ilxys (lambda (ilxy)
		     (message-string "card-to:" image ilxy)
		     (card-to-this-ilxy image ilxy #t)))
    (and undo (gimp-image-undo-group-end image))
    ))


(define (pub-ilxys image)
  ;; return ILXY location(s) of image across all templates
  (let* ((paras (gimp-image-get-parasite-list image))
	 (np (car paras))
	 (pnames (cadr paras))
	 (ilxys nil)
	 )
    ;;(message-string "pnames" pnames)
    (for-each (lambda (pname)
		(when (starts-with? pname PARA-PREFIX)
		      (set! ilxys (cons (parse-card-in pname) ilxys))))
	      pnames)
    ;;(message-string "published ILXYs" ilxys)
    ilxys))

(define (parse-card-in card-in)
  ;; "card-in:i:l:x:y" -> (I L X Y)
  (let* ((nilxy (strbreakup card-in ":"))
	 (image (string->number (nth 1 nilxy)))
	 (layer (string->number (nth 2 nilxy)))
	 (destX (string->number (nth 3 nilxy)))
	 (destY (string->number (nth 4 nilxy))))
    (list image layer destX destY)))

(define (new-global-name name)
  (let* ((n 1)
	 (new-name name))
    (while (and (set! new-name (string-append name ":" (number->string n)))
		;;(car (message-string "new-global-name" new-name n))
		(not (null? 
		      (catch nil (gimp-parasite-find new-name)))))
	   (set! n (+ n 1)))
    new-name))

(define (card-in-ilxy destImage destLayer destX destY)
  ;; keyname in template.xcf
  (let ((sep ":"))
    (string-append PARA-PREFIX (number->string destImage)
		   sep (number->string destLayer)
		   sep (number->string destX)
		   sep (number->string destY))))

(define (ident-of image)
  ;; card-name.png (sans directories)
  (let* ((filename (car (gimp-image-get-filename image)))
	 (basename (car (reverse (strbreakup filename DIR-SEPARATOR)))) ; (card-name.png dir1 dir2 ...)
	 ;; dir1 is presumably "cards" or "publish"
	 ;;(justname (cadr (strbreakup basename "."))) ; card-name png
	 )
    (string-append basename ":" (number->string image))))


(script-fu-register
 "sf-open-card-images" "w-open-cards" "open source files for this template"
 "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31" "RGB* GRAY*"
 SF-IMAGE "Input Image (current image)" 0
 )

(script-fu-menu-register "sf-open-card-images" "<Image>/Script-Fu")

(script-fu-register
 "sf-close-card-images" "w-close-cards" "close source files for this template"
 "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31" "RGB* GRAY*"
 SF-IMAGE "Input Image (current image)" 0
 )

(script-fu-menu-register "sf-close-card-images" "<Image>/Script-Fu")



(define (zap-parasites image drawable)
  ;; detach PARA-PREFIX parasites from this image
  (let* ((ident  (ident-of image))
	 (paras (gimp-image-get-parasite-list image))
	 (np (car paras))
	 (pnames (cadr paras)))
    ;; TODO: from ilxy, zap parasite in template, and global
    (carloop pnames (lambda (pname)
		      (if (starts-with? pname PARA-PREFIX)
			  (gimp-image-detach-parasite image pname))))
    ))

(script-fu-register
 "zap-parasites" "w-zap parasites" "remove parasites for this image"
 "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31" "RGB* GRAY*"
 SF-IMAGE "Input Image (current image)" 0
 SF-DRAWABLE "Input Drawable (layer)" 0
 )

(script-fu-menu-register "zap-parasites" "<Image>/Script-Fu")


(script-fu-register
 "templates-update-image" "w-Templates Update" "update in all templates"
 "Jeff Peck <jeff.peck@pobox.com>" "Jeff Peck" "2019-01-31" "RGB* GRAY*"
 SF-IMAGE "Input Image (current image)" 0
 SF-DRAWABLE "Input Drawable (layer)" 0
 )

(script-fu-menu-register "templates-update-image" "<Image>/Script-Fu")

(gimp-message "loaded script-fu-para-cards")
