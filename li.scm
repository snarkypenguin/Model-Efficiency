;-  Identification and Changes

;--
;	local-intensity.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2009.04.01
;		Location: loki.grayrabble.org:/data/study/playpen/local-intensity.scm
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

;; source-location, source-intensity, plume, angle and eccentricity are time dependent functions, plume-limit is a number
(define (make-local-intensity source-location source-intensity plume plume-limit angle)
  (let* [(plume plume)
		  (source-location source-location)
		  (source-intensity source-intensity)
		  (plume plume)
		  (plume-limit plume-limit)
		  (angle angle)
		  (sqrt2 (if angle (sqrt 2.0) 1.0))
		  (invsqrt2 (/ 1.0 sqrt2))
		  (li-scale 1.0)
		  (li {lambda (t location)
					(let* ((p (list-operator * (/ 4 plume-limit) (list-operator - location (source-location t))))
							 (q (apply + (map * p p)))
							 )
					  (if (< q (sqr plume-limit))
							(* (source-intensity t) (* 1.0 (exp (/ (* -0.5 (apply + (map * p p))) (sqr plume-limit)))))
							0.0))
		  ]
	 
	 (set! li-scale (integrate2d	(lambda (p) (li t p))  (- 0 -plume-limit)  plume-limit  0.01)

	 (lambda (t location)
		
	 
)







(define li-scale #f) ;; THIS IS GETTING SET TO ZERO
(define sl #f)

(define (test-sli t)
  (/ (integrate2d
		(lambda (p) (symmetric-local-intensity t p))
		-plume2 plume2 0.01)
	  li-scale
	  ))

(define (test-ali t)
  (/ (integrate2d*
		(lambda (p) (asymmetric-local-intensity t p))
		-plume2 plume2 0.01 10)
	  li-scale))

(define (sli location)
  (let* ((p (list-operator * (/ 4 plume-limit) (list-operator - location sl)))
			(q (apply + (map * p p)))
			)
	 (if (< q (sqr plume-limit))
		  1.0
		  0.0)))

(define (sli location)
  (let* ((p (list-operator * (/ 4 plume-limit) (list-operator - location sl)))
			(q (apply + (map * p p)))
			)
	 (if (< q (sqr plume-limit))
		  (* (source-intensity 0) (* 1.0 (exp (/ (* -0.5 (apply + (map * p p))) (sqr plume-limit)))))
		  0.0)))
  
;;(define (o-sli location)
;;  (if (not sl) (set! sl (source-location)))
;;  (let* ((p (list-operator * (/ 4 plume-limit) (list-operator - location sl)))
;;			(q (apply + (map * p p)))
;;			)
;;	 (dnl "q = " q)
;;	 (dnl (sqr plume-limit))
;;	 (dnl (* sqrt2 (exp (* -0.5 q))))
;;	 (if (> (sqr plume-limit) q)
;;		  (* sqrt2 (exp (* -0.5 q)))
;;		  0.0)
;;  )
;;)

(define (symmetric-local-intensity t location)
  (if (not li-scale)
		(begin
		  (set! sl (source-location))
		  (set! li-scale (integrate2d sli -plume2 plume2 0.1))

;		  (let ((f (integrate2d* (lambda (p) (symmetric-local-intensity 0 p)) -plume2 plume2 0.01 100)))
;			 (set! li-scale (/ li-scale f)))
		  )
		)
  (let ((p (list-operator * (/ 4 plume-limit) (list-operator - location sl))))

	 (/ (* (source-intensity t) (* 1.0 (exp (/ (* -0.5 (apply + (map * p p))) 
																(sqr plume-limit)))))
		 1.0
;		  (sqrt li-scale)
		  )
	 )
  )


  ;;(exp (* -0.5 (+ (sqr x) (sqr y)))))))
  ;;(/ (integrate2d (lambda (p) (* 2.0 (exp (* -0.5 (apply + (map * p p)))))) '(0 0) '(100 100) 0.001) pi)


;;(define (o-ali location)
;;  (if (not sl) (set! sl (source-location)))
;;  (let* ((p (list-operator * (/ 4 plume-limit) (list-operator - location sl)))
;;			(q (apply + (map * (list sqrt2 invsqrt2) (map * location location))))
;;			)
;;	 (if (> (sqr plume-limit) q)
;;		  (* sqrt2 (exp (* -0.5 q)))
;;		  0.0)
;;	 )
;;)

(define (asymmetric-local-intensity t location)
  (if (not li-scale)
		(begin
		  (set! sl (source-location))
		  (set! li-scale (integrate2d ali -plume2 plume2 0.01))
		  )
		)

  (let ((p (list-operator * (/ 4 plume-limit) (list-operator - location sl))))
	 (/ (* (source-intensity t) (* sqrt2 (exp (* -0.5 (apply + (map * (list sqrt2 invsqrt2) (map * location location)))))))
		 1.0
;		  li-scale
		  )
	 )
  )

;		 (exp (* -0.5 (+ (* sqrt2 (sqr x)) (* invsqrt2 (sqr y))))))))
;		 (/ (integrate2d (lambda (p) (* 2.0 (exp (* -0.5 (apply + (map * p p)))))) '(0 0) '(100 100) 0.001) pi)

;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
