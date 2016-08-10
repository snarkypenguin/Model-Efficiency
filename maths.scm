;-  Identification and Changes

;--
;	maths.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2008.05.04
;		Location: localhost:/usr/home/gray/Study/playpen/maths.scm
;
;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(define pi (acos -1.))
(define 2pi (* 2.0 pi))
(define e (exp 1))
(define sqrt2pi (sqrt 2pi))

(define (fand a . b) (if (null? b) (not (not a)) (and a (apply fand b))))
(define (for a . b) (if (null? b) (not (not a)) (or a (apply fand b))))

(define (count n)
  (let ((f 0))
  (map (lambda (x) (set! f (1+ f)) (1- f)) (make-list n 0))))

(define (plist? a)
  (and (pair? a) (list? a)))

(define (sign a)
  (cond
	((> a 0.0) 1.0)
	((< a 0.0) -1.0)
	(#t 0.0)))


(define (sqr x)
  (* x x))

(define (norm a)
  (if (number? a)
		(* a a)
		(apply + (map * a a))))

(define (v-length a)
  (if (number? a)
		(abs a)
		(if (plist? a) 
			 (sqrt (norm a))
			 'bad-argument)))

(define (distance p q)
  (if (and (number? p) (number? q))
		(abs (- p q))
		(let ()
		  (if (number? p) (set! p (list p)))
		  (if (number? q) (set! q (list q)))

		  (sqrt (apply + (map sqr (map - p q)))))))

(define (list-operator op p1 p2)
	 (cond
	  ((and (number? p1) (number? p2))
		(op p1 p2))
	  ((and (number? p1) (list? p2))
		(list-operator op (make-list (length p2) p1) p2))
;		(map (lambda (x) (list-operator op p1 x)) p2))
	  ((and (list? p1) (list? p2) (eq? (length p1) (length p2)))
		(map op p1 p2))
	  ((and (number? p2) (list? p1))
		(list-operator op p1 (make-list (length p1) p2)))
;		(map (lambda (x) (list-operator op x p2)) p1))
	  (else 
		(dnl "list-operator is confused!")
		(dnl "... p1 = " p1)
		(dnl "... p2 = " p2)
		#f)
	  )
	 )


(define (dot a b)
  (apply + (list-operator * a b)))


(define (rotated-vector v theta)
  (if (eq? (length v) 1.0)
		v
		(list (- (* (car v) (cos theta)) (* (cadr v) (sin theta)))
				(+ (* (cadr v) (cos theta)) (* (car v) (sin theta)))) ))

;; Composition operator
(define (o . funlist)
  (if (eq? (length funlist) 1)
      (lambda x (apply (car funlist) x))
      (lambda x ((car funlist) (apply (apply o (cdr funlist)) x)))))


(define (change-basis vect basis origin)
  (let ((n (length basis)))
	 (if (null? origin) (set! origin (make-list (length basis) 0.0)))

	 (let* ((r (list-operator - basis origin))
			  (s (list-operator - vect origin))
			  (v (list-operator - s r))
			  (theta '())
			  )
		(if (eq? n 1)
			 v
			 (rotated-vector v (- 0 (atan (car r) (cadr r))))))))


; a and b are vectors ... usually used as (projection (list-op - s r) (list-op - t r))
(define (projection a b)
  (/ (dot a b) (v-length b)))


(define (abeam a b scale) ; scale might be the distance covered  in a timestep
  (let* ((v (projection a b))
			(w (/ v scale))
			)
	 v))


(define (mult2 x  y)
  (cond
	((and (plist? x) (plist? y) (map * x y)))
	((plist? x) (map (lambda (p) (* p y)) x))
	((plist? y) (map (lambda (p) (* x p)) y))
	((and (number? x) (number? y)) (* x y))
	(#t (#f 'bad-argument))
	))
				
(define (mult x . y)
  (cond
	((null? y) x)
	((eq? (length y) 1.0) (mult2 x (car y)))
	(#t (mult2 x (apply mult y)))))

(define (div x y)
  (cond
	((and (plist? x) (plist? y) (map / x y)))
	((plist? x) (map (lambda (p) (/ p y)) x))
	((plist? y) (map (lambda (p) (/ x p)) y))
	((and (number? x) (number? y)) (/ x y))
	(#t (#f 'bad-argument))
	))

(define (add2 x  y)
  (cond
	((and (plist? x) (plist? y) (map + x y)))
	((plist? x) (map (lambda (p) (+ p y)) x))
	((plist? y) (map (lambda (p) (+ x p)) y))
	((and (number? x) (number? y)) (+ x y))
	(#t (#f 'bad-argument))
	))

(define (add x . y)
  (cond
	((null? y) x)
	((eq? (length y) 1.0) (add2 x (car y)))
	(#t (add2 x (apply add y)))))

(define (sub2 x  y)
  (cond
	((and (plist? x) (plist? y) (map - x y)))
	((plist? x) (map (lambda (p) (- p y)) x))
	((plist? y) (map (lambda (p) (- x p)) y))
	((and (number? x) (number? y)) (- x y))
	(#t (#f 'bad-argument))
	))

(define (sub x . y)
  (cond
	((null? y) (sub2 0 x))
	((eq? (length y) 1.0) (sub2 x (car y)))
	(#t (sub2 x (apply add y)))))

(define (make-pprnd m)
  (let ((table (make-vector 1024 0.0))
		  (mean m)
		  (size 0)
		  )
	 (let loop ((i 0))
		(if (and (eq? size 0) (< i 1024))
			 (begin
				(vector-set! table i (- 1.0 (exp (/ (* -1.0 i) mean))))
				(if (> (vector-ref table i) 0.9999) (begin (set! size (1+ i)) (vector-set! table size 1.0)))
				(loop (1+ i)))))
	 
	 (lambda mode
		(cond
		 ((and (plist? mode) (eq? (car mode) 'mean))
		  mean)
		 ((and (plist? mode) (eq? (car mode) 'size))
		  size)
		 (#t (let (
;					  (r (random:uniform))
					  (r (random-real))
					  )
				 (let loop ((i 0))
					(if (null? mode)
						 (cond
						  ((>= i size) size)
						  ((<= r 0.0) 0)
						  ((< r (vector-ref table i))
								 i)
						  (#t
							(loop (1+ i)) )
						  )
						 (cond
						  ((>= i size) 1.0)
						  ((<= r 0.0) 0)
						  ((< r (vector-ref table i))
								 (/ i size))
						  (#t
							(loop (1+ i)) )
						  )
						 )
					)
				 )
			  )
		 )
		)
	 )
  )


(define (pow e x)
  (exp (* x (log e))))

(define distance-limit-scale 64.0) ;; range at which the integral of (vector-weight r)  reaches 99% of its maximum over [0, oo)
;; vector-weight reaches 99% of its value when the location-vector has a length of 1
(define (vector-weight location-vector)
  (let* ((sx (v-length location-vector))
			(x (/ sx distance-limit-scale)) 
			)
	 (if (> x 1.0) 
		  0.0 
		  (/ 1.0 (+ (* x x) (pow (- 1.0 (/ x (+ x 1.0))) 0.01))))))


(define (extend-arith-op-to-funcs op) 
  (lambda args
	 (if (apply fand (map number? args)) ;; This way numbers work as they usually do
		  (apply op args)
		  (lambda (x)
			 (apply op (map (lambda (f) (if (procedure? f) (f x) f)) args))))))

;; Doubled so we don't get them accidentally
(define ++ (extend-arith-op-to-funcs +))
(define -- (extend-arith-op-to-funcs -))
(define ** (extend-arith-op-to-funcs *))
(define // (extend-arith-op-to-funcs /))

;; This returns a piecewise linear function of one argument which is zero outside its domain
(define (pwl ptlist)
  (lambda (x)
	 (if (or (null? ptlist) (not (pair? ptlist)) (not (pair? (car ptlist))) (< x (caar ptlist))) 
		  0.0
		  (let hunt ((p ptlist))
			 (cond
			  ((null? p) 0.0)
			  ((and (pair? (cdr p)) (< x (caadr p)))
				(let ((d (caar p))
						(D (caadr p))
						(n (cadar p))
						(N (cadadr p)))
				  (+ (* (/ (- N n) (- D d)) (- x d)) n)))
			  (#t (hunt (cdr p))))))))

;; I think that this might now be superfluous
(define (interpolate pwl x)
  (cond
	((null? pwl)  #f)
	((not (pair? pwl)) #f)
	((<= x (caar pwl)) (cadar pwl))
	((null? (cdr pwl)) (cadar pwl))
	((< x (caadr pwl)) 
	 (let* ((p1 (car pwl))
			  (p2 (cadr pwl))
			  (a (car p1))
			  (m (cadr p1))
			  (b (car p2))
			  (M (cadr p2)))
		(if (< (abs (- b a)) 1e-80)
			 (/ (+ m M) 2.0)
			 (+ (* (/ (- x a)
						 (- b a))
					 (- M m)
					 )
				 (* 1.0 m)
				 )))
	 )
	(#t (interpolate (cdr pwl) x)))
)

;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
