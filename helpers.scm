;-  Identification and Changes

;--
;	helpers.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2010.06.12
;		Location: loki:/data/study-runs/playpen/helpers.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2010 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(define WORDY #f)
(define DNL-debugging #f)

(define (Display arg . port)
  (if (pair? arg)
		(apply write (append (list arg) port))
		(apply display (append (list arg) port))))
		

(define (DDNL . args)
  (if DNL-debugging
		(let ()
		  (map Display args)
		  (newline))
		#t)
)

(define (and? . args)
  (cond ((null? args) #t)
	((car args) (apply and? (cdr args)))
	(else #f)))

(define (or? . args)
  (cond ((null? args) #f)
	((car args) #t)
	(else (apply or? (cdr args)))))

(define (d* . args)
  (map Display args)
  )

(define (dsp* . z)
  (Display (car z)) 
  (let loop ((pz (cdr z))) 
	 (if (not (null? pz) )
		  (begin 
			 (Display " ") 
			 (Display (car pz)) 
			 (loop (cdr pz)))))
  )

(define (wdnl . args)
  (if WORDY (apply dnl args)))


(define (f-d* file . args)
;  (let ((f (open-output-file file "w")))
  (let ((f (open-output-file file)))
	 (map (lambda (x) (Display f x)) args)
	 (close-output-port f)
	 )
  )


(define (dnl . args)
  (map Display args)
  (newline))

(define DNL-DEBUG #f)

(define (DNL . args)
  (if DNL-DEBUG
		(let()
		  (map Display args)
		  (newline))))

(define (f-dnl file . args)
;  (let ((f (open-output-file file "w")))
  (let ((f (open-output-file file)))
	 (map (lambda (x) (Display f x)) args)
	 (newline f)
	 (close-output-port f)
	 )
  )


(define (dnlsp . z)
  (Display (car z)) 
  (let loop ((pz (cdr z))) 
	 (if (null? pz) 
		  (newline) 
		  (begin 
			 (Display " ") 
			 (Display (car pz)) 
			 (loop (cdr pz))))))


(define (f-dnlsp file . z)
  (let ((f (open-output-file file)))
	 (Display f (car z)) 
	 (let loop ((pz (cdr z))) 
		(if (null? pz) 
			 (newline f) 
			 (begin 
				(Display f " ") 
				(Display f (car pz)) 
				(loop (cdr pz)))))
	 (newline f)
	 (close-output-port f)))


(define (write-line obj . port)
  (if (null? port) (set! port (current-output-port)) (set! port (car port)))
  (Display obj port)
  (newline port))

(define (write-line* obj file)
  (if (or (not (list? obj)) (null? obj) (eq? (length obj) 1))
		(write-line obj file)
		(let ()
		  (Display (car obj) file)
		  (map (lambda (x) (Display " " file) (Display x file)) (cdr obj))
		  (newline file))))



(define (character? x) (char? x))
(define (1+ x) (+ x 1))
(define (1- x) (- x 1))
(define (close f) (close-port f))
(define (acons key val lst)
  (cons (cons key val) lst))

(define (make-list n . init)
  (if (<= n 0) 
		'()
		(if (null? init)
			 (cons '() (make-list (1- n)))
			 (cons (car init) (make-list (1- n) (car init))))))

;(define (map-right proc arg left right)
;  (map (lambda (x) (proc left x)) right))
;
;(define (map-left proc arg left right)
;  (map (lambda (x) (proc x right)) left))


(define (list-set! l i v)
  (if (zero? i)
      (set-car! l v)
      (list-set! (cdr l) (1- i) v)))

(define  (make-name num)
  (let ((tag ""))
	 (cond
	  ((number? num) 
		(set! tag (cond 
					  ((>= 1000 num) (number->string num))
					  ((>= 100 num) (string-append "0" (number->string num)))
					  ((>= 10 num) (string-append "00" (number->string num)))
					  ((>= 1 num) (string-append "000" (number->string num)))
					  )))
	  ((symbol? num) (set! tag (symbol->string num)))
	  ((string? num) (set! tag num))
	  ((character? num) (set! tag (list->string (list num))))
	  )
	 
	 (set! id (+ 1 id))
	 (string-append "entity_" tag ":" (number->string id))
	 )
  )

(define (list-op op p1 p2)
  (list-operator op p1 p2))


(define (range min max step)
  (cond
	((eq? min max) (list min))
	((and (< min max) (< step 0))
	 (range min max (- step)))

	((and (> min max) (> step 0))
	 (range max min step))

	((and (> min max) (< step 0))
	 (let loop ((l (list min))
					(x (+ min step)))
		(if (< x max)
			 l
			 (loop (append l (list x)) (+ x step)))))
	((and (< min max) (> step 0))
	 (let loop ((l (list min))
					(x (+ min step)))
		(if (> x max)
			 l
			 (loop (append l (list x)) (+ x step)))))
	(else 'bad-range)))

(define (random-angle)
  (* pi (- (random 2.0) 1)))

(define (rotated-velocity v theta)
  (rotated-vector v theta))


;; remove an object from a list
(define (remove obj lst)
  (letrec ((head (list '*head*)))
    (letrec ((remove
               (lambda (lst tail)
                 (cond ((null? lst)
								lst)
							  ((not (pair? lst))
								#f)
                       ((eqv? obj (car lst)) (remove (cdr lst) tail))
                       (#t
                        (set-cdr! tail (list (car lst)))
                        (remove (cdr lst) (cdr tail)))))))
      (remove lst head))
    (cdr head)))

;; remove a set of objects from the list
(define (remove-if pred? lst)
  (let remove-if ((lst lst) (result '()))
    (cond ((null? lst) (reverse result))
          ((pred? (car lst)) (remove-if (cdr lst) result))
          (else
           (remove-if (cdr lst) (cons (car lst) result))))))



;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
