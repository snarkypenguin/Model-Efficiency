;-  Identification and Changes

;--
;	across-run-stats.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2009.05.03
;		Location: Loki:/data/study/playpen/across-run-stats.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2009 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 



(define (open-files lst)
  (map (lambda (x) (open-input-file x)) lst))

(define (read-lines-in-files fdlst)
  (map (lambda (x) (read-line x)) fdlst))

(define (read-whole-files fdlst)
  (let loop ((l '()))
	 (if (not (null? l))
		  #f #f)))

(define (close-files fdlst)
  (map (lambda (x) (close-port x)) fdlst))

(define (get-mean raw-time-data-array)
  (map (lambda (x) 
			(let ((t (car x))
					(v (cdr x)))
			  (cons t (list (/ (apply + v)) (length v)))))
		 raw-time-data-array))

(define (get-stddev raw-time-data-array mean-time-data-array)
  (map (lambda (X M) 
			(let ((t (car X))
					(v (cdr X))
					(m (cadr M)))
			  (if (not (equal? t (car M))) (#f "times dont match!"))
			  (cons t (list (sqrt (/ (apply + (map (lambda (y) (let ((s (- y m))) (* s s))) v)) (length v)))))
			  ))
		 raw-time-data-array mean-time-data-array))

(define (do-contaminant-levels fdlist alist)
  #f)


;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
