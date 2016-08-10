;;======================================================================;
;;==                    Define stats collecting stuff                 ==;
;;======================================================================;


(require 'sort)

(define (real->integer f)
  (inexact->exact (truncate f)))

(define (stats-bin . args)
  (let ((mode 'accurate) ;; fast or accurate
		  (flags args)
		  (fmin 1e+99)
		  (fmax -1e+99)
		  (mean 0)
		  (epsilon 0.0)
		  (variance 0.0)
		  (n 0.0)
		  (ssq 0.0)
		  (sum 0.0)
		  (data '())
		  (sorted #f)
		  )

    (define (add-number x . i)
      (if (member 'keepdata args) (set! sorted #f) (set! sorted #t))
      (if (null? i)
			 (set! i 1)
			 (set! i (car i)))

      (set! fmin (min x fmin))
      (set! fmax (max x fmax))

      (if (<= i 1)
			 (begin
				(set! n (1+ n))

				(set! ssq (+ ssq (* x x)))
				(set! sum (+ sum x)) 

				(if (member 'keepdata args) (set! data (cons x data)))
				(set! epsilon (/ (- x mean) n))

				(cond
				 ((eq? mode 'fast)
				  (set! mean (/ sum n))
				  (set! variance (/ (- ssq (* n mean mean)) n) ))
				 (#t ;; 'accurate
				  (set! variance (+ (* (/ (- n 1) n) variance) 
										  (* epsilon epsilon (- n 1))))
				  (set! mean (+ epsilon mean)) )) )

			 (begin
				(set! n (+ n i))

				(set! ssq (+ ssq (* x x i)))
				(set! sum (+ sum (* x i)))

				(if (member 'keepdata args) (set! data (append (make-list i x) data)))

				(cond
				 ((eq? mode 'fast)
				  (set! epsilon (/ (- x (/ (- sum x) (1- n)) n)))
				  (set! mean (/ sum n))
				  (set! variance (/ (- ssq (* n  mean mean)) n) ))
				 (#t
				  (let statloop ((j 0))
					 (if (< j i)
						  (begin
							 (set! epsilon (/ (- x mean) n))
							 (set! variance (+ (* (/ (- n 1) n) variance) 
													 (* epsilon epsilon (- n 1))))
							 (set! mean (+ epsilon mean)) 
							 (statloop (1+ j)))))) ))) )

    (define (centile n)
      (centile-mM n #f #f))

    (define (centile-0 n)
      (centile-mM n 0 #f))

    (define (centile-mM centage m M)
		(if (member 'keepdata args) 
			 (let ()
				(if (> (abs centage) 1.0)
					 (set! centage (/ centage 100.0)))

				(if (zero? n)
					 0
					 (let ((d (sort! 
								  (list-except (lambda (x) (if (number? M) (>= x M) M))
													(list-except (lambda (x) (if (number? m) (<= x m) m)) data)
													)
								  <))
							 )
						(list-ref d (real->integer (* (length d) centage)))
						)))
			 #f))

    (define (median-mM m M)
      (centile-mM 0.5 m M))

    (define (median-0)
      (median-mM 0 #f))

    (define (median)
      (median-mM #f #f))



    (lambda d
      (cond
       ((or (null? d) (equal? (car d) 'list))
		  (if (member 'keepdata args) (set! data (sort! data <)))
		  (set! sorted #t)
		  (list 
			(cons 'stat:data (if (member 'keepdata args) data 'none-available))
			(cons 'stat:mode mode)
			(cons 'stat:n n) 
			(cons 'stat:mean mean)
			(if (<= n 0)
				 (cons 'stat:median (if (member 'keepdata args) 0 'none-available))
				 (cons 'stat:median (if (member 'keepdata args) (median) 'none-available)))
			(if (<= n 0)
				 (cons 'stat:median-0 (if (member 'keepdata args) 0 'none-available))
				 (cons 'stat:median-0 (if (member 'keepdata args) (median-0) 'none-available)))
			(cons 'stat:min fmin)
			(cons 'stat:max fmax)
			(cons 'stat:sum sum)
			(cons 'stat:sum_x sum)
			(cons 'stat:sum_x-min (- sum (* n fmin)))
			(cons 'stat:sum_max-x (- (* n fmax) sum))
			(cons 'stat:sum_xx ssq)
			(cons 'stat:variance variance)
			(cons 'stat:variance-1 
					(if (> n 0) (/ (* variance n) (1- n)) 0))
			(cons 'stat:stddev (sqrt variance))))

       ((number? (car d))
		  (add-number (car d)))

;;       ((and (or (symbol? (car d)) (string? (car d))) 
;;				 (string-match 
;;				  "\(centile\|median\)=.*" 
;;				  (if (symbol? (car d)) (symbol->string (car d)) (car d))))
;;		  (let ((m #f)
;;				  (M #f)
;;				  (c 0.5)
;;				  (key (car (string-split (if (symbol? (car d)) (symbol->string (car d)) (car d)) #\=)))
;;				  (args (string-split (cadr (string-split (if (symbol? (car d)) (symbol->string (car d)) (car d)) #\=)) #\,))
;;				  )
;;			 (if (string=? key "median=")
;;				  (begin
;;					 (set! c 0.5)
;;					 (if (>= (length args) 1) 
;;						  (set! m (car args)))
;;					 (if (>= (length args) 2) 
;;						  (set! M (cadr args)))
;;					 )
;;				  (begin ; centile=
;;					 (set! c (car args))
;;					 (if (eq? (length args) 2) 
;;						  (set! m (cadr args)))
;;					 (if (eq? (length args) 3)
;;						  (begin
;;							 (set! m (cadr args))
;;							 (set! M (caddr args))))
;;					 )
;;				  )
;;			 
;;			 (centile-mM (if c (dstring->number c) c) (if m (dstring->number m) m) (if M (dstring->number M) M))
;;			 ) )
       ((symbol? (car d))
		  (case (car d)
			 ('clear
			  (set! fmin 1e+99)
			  (set! fmax -1e+99)
			  (set! mean 0)
			  (set! epsilon 0.0)
			  (set! variance 0.0)
			  (set! n 0.0)
			  (set! ssq 0.0)
			  (set! sum 0.0)
			  (set! data '())
			  (if (member 'keepdata args) (set! sorted #f) (set! sorted #t))
			  )

			 ('data (if (member 'keepdata args) data 'none-available))
			 ('mode mode)
			 ('n n)
			 ('mean mean)
			 ('median (median))
			 ('median-0 (median-0))
			 ('median-mM (apply median-mM (cdr d)))
			 ('centile (apply centile (cdr d)))
			 ('centile-0 (apply centile-0 (cdr d)))
			 ('centile-mM (apply centile-mM (cdr d)))
			 ('min fmin)
			 ('max fmax)
			 ('sum sum)
			 ('sum_x sum)
			 ('sum_x-min (- sum (* n fmin)))
			 ('sum_max-x (- (* n fmax) sum))
			 ('sum_xx ssq)
			 ('variance variance)
			 ('variance-1 (if (> n 0) (/ (* variance n) (1- n)) 0))
			 ('stddev (sqrt variance))

			 ('fast (set! mode 'fast))
			 ('accurate (set! mode 'accurate))
			 ('fill-to 
			  (if (< (length d) 2)
					'error-in-stat-bin
					(let ((i (- n (cadr d)))
							(v (if (= (length d) 2) 0 (caddr d))))
					  (if (> i 0) 
							(add-number v i)))))
			 ('add
			  (if (< (length d) 2)
					'error-in-stat-bin
					(let ((i (cadr d))
							(v (if (= (length d) 2) 0 (caddr d))))
					  (if (> i 0) 
							(add-number v i)))))
			 (else (list 'unimplemented-stat d))))
       (#t #f) )) ))


(define (stats-bins multi)
  (let ((bins '())
		  )
    (let loop ((b (reverse (map car multi))))
      (if (not (null? b))
			 (begin
				(set! bins (acons (car b) (stats-bin) bins))
				(loop (cdr b))))
      )

    (lambda d
      (cond
       ((null? d)
		  (let dump-stat ((b bins))
			 (if (not (null? b))
				  (begin 
					 ((car b))
					 (dump-stat (cdr b))))))
       ((equal? (car d) 'debug)
		  bins)
       ((number? (car d))
		  (dnl "Needs more than a number boikie!")
		  )

       ((list? (car d))
		  (set! d (car d))
		  (let load-stat ((b bins) (dd d))
			 (if (and (not (null? dd)) (not (null? b)))
				  (begin 
					 ((cdar b) (car dd))
					 (load-stat (cdr b) (cdr dd)) )
				  #f))
		  )
       ((symbol? (car d))
													 ; Decode it and pass it on
		  (if (null? (cdr d))
				(map (lambda (x) ((cdr x) (car d))) bins)
				(if (eq? (length d) 2)
					 (let ((b (assoc (cadr d) bins)))
						(if b ((cdr b) (car d)) #f))
					 (map (lambda (x) 
							  (let ((b (assoc x bins)))
								 (if b ((cdr b) (car d)) #f))) (cdr d))
					 )
				)
		  )
       (#t #f) )
      )
	 ))

