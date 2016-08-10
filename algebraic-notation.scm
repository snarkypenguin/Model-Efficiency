;-  Identification and Changes

;--
;	algebraic-notation.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2008.05.20
;		Location: localhost:/usr/home/gray/Study/playpen/algebraic-notation.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2008 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(define (high-infix-op? lst)
  (cond
	((null? lst) #f)
	((and (list? lst) (member (car lst) '(* /))) #t)
	((member lst '(* /)) #t)
	(#t #f)))

(define (low-infix-op? lst)
  (cond
	((null? lst) #f)
	((and (list? lst) (member (car lst) '(+ -))) #t)
	((member lst '(+ -)) #t)
	(#t #f)))

(define (infix-operator? lst)
  (cond
	((null? lst) #f)
	((and (list? lst) (member (car lst) '(+ - * /))) #t)
	((member lst '(+ - * /)) #t)
	(#t #f)))

(define (*-infix-operator? lst)
  (and (infix-operator? lst)
		 (eq? (car lst) '*)))

(define (maths-function? lst)
  (cond
	((null? lst) #f)
	((member lst '(sqrt exp ln log sin cos tan atan sqr distance length norm)) #t)
	((and (list? lst) (member (car lst) '(sqrt exp ln log sin cos tan atan sqr distance length norm))) #t)
	(#t #f)))
  
(define (list-op? lst)
  (cond
	((atom? lst) #f)
	(#t (eq? (car lst) 'list-op))))

(define (distribute-operator lst)
  (display "Looking at ")(display lst)(newline)
  (let ((result 
			(let ((op (car lst))
					(args (cdr lst)))
			  (if (null? args)
					lst
					(let loop ((remainder args))
					  (if (<= (length remainder) 1)
							remainder
							(append (list (car remainder)) (list op) (loop (cdr remainder))))))
			  ))
		  )
	 (cond
	  ((atom? result)
		result)
	  ((null? (cdr result))
		(car result))
	  (#t result))))



(define (do-expression sexp)
  (let ((result 
			(cond
			 ((atom? sexp)
			  sexp)
			 ((list? sexp)
			  (let ((f (car sexp))
					  (a (map (lambda (x) (do-expression x)) (cdr sexp)))
					  )
				 (cond
				  ((infix-operator? (append (list f) a) )
					(distribute-operator (append (list f) a)))
				  ((list-op? sexp)
					(do-expression a))
				  ((maths-function? f)
					(if (or (atom? a) (null? (cdr a)))
						 (list f (car a))
						 (list f a)))
				  (#t sexp)))))))
	 (cond
	  ((atom? result)
		result)
	  ((and (eq? (length result) 1)
			  (list? (car result)))
		(car result))
	  (#t result))))

(define (infix)
  (do-expression (read)))








;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
