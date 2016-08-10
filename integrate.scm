;-  Identification and Changes

;--
;	integrate.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2008.08.03
;		Location: trickster.100acwood.grayrabble.org:/home/gray/Study/playpen/integrate.scm
;
;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(load "maths.scm")


(define debugging-integration #f)

(define MAX-DEPTH 5)

;; Algorithm translated from Wikipedia: "Adaptive Simpson's Method" 30/04/2009

(define (inner-adaptive-integrate f a b eps estimate fa fc fb k)
  (if (< b a) 
		(- 0 (inner-adaptive-integrate f b a eps estimate fb fc fa k))
		(let* ((h (- b a))
				 (c (/ (+ a b) 2.0))
				 (d (/ (+ a c) 2.0))
				 (e (/ (+ c b) 2.0))
				 (fd (f d))
				 (fe (f e))
				 (left-estimate (* (/ h 12.0) (+ fa (* 4.0 fd) fc)))
				 (right-estimate (* (/ h 12.0) (+ fc (* 4.0 fe) fb)))
				 (inner-estimate (+ left-estimate right-estimate))
				 ;;				 (delta (/ (- inner-estimate estimate) 15.0))
				 )
		  (if (or (<= k 0) (<= (abs (- inner-estimate estimate)) (* 15.0 eps)))
				(+ inner-estimate (/ (- inner-estimate estimate) 15.0))
				(+ (inner-adaptive-integrate f a c (/ eps 2.0) left-estimate fa fd fc (1- k))
					(inner-adaptive-integrate f c b (/ eps 2.0) right-estimate fc fe fb (1- k))
					)))))

(define (adaptive-integrate f a b eps k)
  (if (< b a)
		(- 0 (adaptive-integrate f b a eps k))
		(let* ((c (/ (+ a b) 2.0))
				 (fa (f a))
				 (fc (f c))
				 (fb (f b))
				 (estimate (+ fa (* 4 fc) fb))
				 )
		  (inner-adaptive-integrate f a b eps estimate fa fc fb k))))

  
(define (integrate f a b eps . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (adaptive-integrate f a b eps k)
  )

(define (integrate% f a b eps ignore-ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate f a b eps k))

(define (integrate* f a b eps ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (if (zero? ss) 
		(integrate f a b eps k)
		(let loop ((sum 0)
					  (x a))
		  (if (>= (+ x ss) b)
				(+ sum (integrate f x b eps k))
				(loop (+ sum (integrate f x (+ x ss) eps k))
						(+ x ss))))))


;; a is the lower left corner of a rectangular domain, b is the upper right corner

(define (integrate2d func a b eps . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate (lambda (x) (integrate (lambda (y) (func (list x y))) (cadr a) (cadr b) eps)) (car a) (car b) eps))

(define (integrate2d* func a b eps ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate* (lambda (x) (integrate* (lambda (y) (func (list x y))) (cadr a) (cadr b) eps ss k)) (car a) (car b) eps ss k))


(define (integrate2d% f a b eps ignore-ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate2d f a b eps k))

(define (integrate2d%* f a b eps ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate% (lambda (x) 
					 (integrate* 
					  (lambda (y) (apply f x y)) 
					  (cadr a) (cadr b) eps ss k)) 
				  (car a) (car b) eps ss k) 
  )


(define (integrate2d*% f a b eps ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate* (lambda (x) 
					 (integrate%
					  (lambda (y) (apply f x y)) 
					  (cadr a) (cadr b) eps ss k)) 
				  (car a) (car b) eps ss k) 
  )


(define (integrate2d** f a b eps ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate* (lambda (x) 
					 (integrate* 
					  (lambda (y) (apply f x y)) 
					  (cadr a) (cadr b) eps ss k)) 
				  (car a) (car b) eps ss k) 
  )


(define (test-integrate2d p)
	 (if (<= (apply + (map (lambda (x) (* x x)) p)) 1.0) 1.0 0.0))

(define (inner-general-adaptive-integrate f a b eps estimate fa fc fb << ** // ++ -- k)
  (if (not a) (#f 'this))
  (if (not b) (#f 'that))
  
  (if (< eps 0.0) (set! eps (abs eps)))
  (if (<< b a) 
		(- (inner-general-adaptive-integrate f b a eps k << ** // ++ -- k))
		(let* ((h (v-length (-- b a)))
				 (c (// (++ a b) 2.0))
				 (d (// (++ a c) 2.0))
				 (e (// (++ c b) 2.0))
				 (fd (f d))
				 (fe (f e))
				 (left-estimate (* (/ h 12.0) (+ fa (* 4.0 fd) fc)))
				 (right-estimate (* (/ h 12.0) (+ fc (* 4.0 fe) fb)))
				 (inner-estimate (+ left-estimate right-estimate))
				 ;;				 (delta (/ (- inner-estimate estimate) 15.0))
				 )

;		  (mdnl "(inner-general-adaptive-integrate f" a b eps estimate fa fc fb k ")")
;		  (mdnl "inner estimate =" inner-estimate)
;		  (mdnl "k =" k)
;		  (mdnl "(abs (- inner-estimate estimate)) =" (abs (- inner-estimate estimate)))
;		  (mdnl "(* 15 eps) =" (* 15 eps))
		  (if (or (<= k 0) (<= (abs (- inner-estimate estimate)) (* 15.0 eps)))
				(+ inner-estimate (/ (- inner-estimate estimate) 15.0))
				(+ (inner-general-adaptive-integrate f a c (/ eps 2.0) left-estimate fa fd fc << ** // ++ -- (1- k))
					(inner-general-adaptive-integrate f c b (/ eps 2.0) right-estimate fc fe fb << ** // ++ -- (1- k))
					)
				)
		  )
		)
  )

(define (general-adaptive-integrate f a b eps swap-order mult div add sub . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (let (
		  (<< swap-order)
		  (** mult)
		  (// div)
		  (++ add)
		  (-- sub)
		  )

;	 (->list a)
;	 (->list b)

	 (if (< eps 0.0) (set! eps (abs eps)))

	 (if (<< b a)
		  (- 0 (general-adaptive-integrate f b a eps swap-order mult div add sub k))
		  (let* ((c (// (++ a b) 2.0))
					(fa (f a))
					(fc (f c))
					(fb (f b))
					(estimate (+ fa (* 4 fc) fb))
					)
			 (inner-general-adaptive-integrate f a b eps estimate fa fc fb swap-order mult div add sub k)
			 )
		  )
	 )
)

(define (general-integrate f a b eps swap-order mult div add sub . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (general-adaptive-integrate f a b eps swap-order mult div add sub k)
)


; This is included as a test of integrate-RV
(define (integrate-R f a b eps . k)
; function over the field of real numbers
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (general-integrate f a b eps < * / + - k))

(define (integrate-R% f a b eps ignore-ss . k)
; function over the field of real numbers
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate-R f a b eps k))

(define (no-order-swap a b)
  #f)

;;;; integrates over a path in a vector space -------------------------

(define (integrate-RV f a b eps . k)
; function over a line in a vector space

  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (if (and (number? a) (number? b))
		(integrate-R f a b eps k)
		(general-integrate f a b eps no-order-swap mult div add sub k)
		)
  )

(define (integrate-RV% f a b eps ignore-ss . k)
; function over a line in a vector space
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate-RV f a b eps k))

(define (integrate-RV* f a b eps ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (let* ((d (list-operator - b a))
;			(x1 (dnl "d = " d))
			(HdH (v-length d))
;			(x2 (dnl "HdH = " HdH))
			(dx (list-operator * ss (list-operator / d HdH)))
;			(x3 (dnl "dx = " dx))
			)
	 (if (<= HdH eps) 
		  (integrate-RV f a b eps k)
		  (let loop ((sum 0.0)
						 (x a))
			 (if (>= (v-length (list-operator - (add x dx) a)) HdH)
				  (+ sum (integrate-RV f x b eps k))
				  (loop (+ sum (integrate-RV f x (add x dx) eps k))
						  (add x dx)))))))
					

;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***








