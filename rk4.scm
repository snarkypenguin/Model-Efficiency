;-  Identification and Changes

;--
;	rk4.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2008.08.03
;		Location: trickster.100acwood.grayrabble.org:/home/gray/Study/playpen/rk4.scm
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

;
;  dy/dt = f(x,y) with  y(x ) = y
;                          0     0
;  where f(x,y) is some functional form using
;  y() (and possibly x)
;
;  Recall that the Runge-Kutta process needs an x  and 
;                                                0 
;  a y  in order to start
;     0
;  and progresses thus:
;
;  k = f(x , y )
;   1     n   n
;	  
;  k = f(x + h/2, y + k h/2)
;   2     n        n   1	 
;		     
;  k = f(x + h/2, y + k h/2)
;   3     n        n   2	 
;		         
;  k = f(x + h, y + k h)    
;   4     n      n   3
;
;   y   = y  + ---(k + 2k + 2k + k )
;    n+1   n    6   1    2    3   4
;

; dy/dx is the function to be integrated (the integral being y(x)
; xt is an upper bound on x ... the value you want, namely y(xt)
; h is the size of steps as a proportion of (xt - x0)
; x0 is the lower bound of the integral
; y0 is the value of y(x0)

(define (rk4 dy/dx xt h x0 y0 )
  (set! h (* h (- xt x0)))
  (let ((y y0)
		  (NI (1+ (/ (- xt x0) h)))
		  (h/2 (/ h 2.0))
		  (h/6 (/ h 6.0))
		  (ly 0.0)
		  (lx 0.0)
		  (f dy/dx) 
		  )
	 (let loop ((x x0))
; do not step past the end
		(if (< xt (+ x h))
			 (set! h (- xt x)))

		(if (< x xt)
			 (let* (
					  (k1 (f x y))
					  (k2 (f (+ x h/2) (+ y (* k1 h/2))))
					  (k3 (f (+ x h/2) (+ y (* k2 h/2))))
					  (k4 (f x (+ y (* k3 h))))
					  (yy  (+ y  (* h/6 (+ k1  (* 2.0 (+ k2  k3)) k4))))
					  )
				
				(set! ly y)
				(set! lx x)
				(set! x (+ x h))
				(set! y yy)
				(loop x))
			 (+ (* (/ (- y ly) (- x lx)) (- xt lx)) ly)
			 )
		)
	 )
  )



;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
