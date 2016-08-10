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




;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
