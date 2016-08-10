#!/usr/bin/env gsi-script
;-  Identification and Changes

;--
;	postprocessing.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2009.05.03
;		Location: Loki:/data/study/playpen/load-contaminant-file.scm
;
;	History:
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(load "utils.scm")

;;; (define (read-c-line f) ***
;;;   (let* ((who (read f)) ***
;;; 			(t (read f)) ***
;;; 			(dt (read f)) ***
;;; 			(contact (read f)) ***
;;; 			(contamination (read f)) ***
;;; 			(N (read f)) ***
;;; 			(intensity-t (read f)) ***
;;; 			(intensity-t+dt/2 (read f)) ***
;;; 			(intensity-t+dt (read f)) ***
;;; 			) ***

;;; 	 (let ((v (list t contact contamination N))) ***
;;; 		(if (member #!eof v) ***
;;; 			 #!eof ***
;;; 			 v)))) ***


;;; (define (contaminant-data filename) ***
;;;   (let ((data '()) ***
;;; 		  (f (open-input-file filename)) ***
;;; 		  ) ***
	 
;;; 	 (let loop ((v (read-c-line f)) ***
;;; 					) ***
;;; 		(if (list? v) ***
;;; 			 (begin ***
;;; 				(set! data (append-assoc! (car v) (cons (cdr v) '()) data)) ***
;;; 				(loop (read-c-line f))) ***
;;; 			 ) ***
;;; 		) ***
;;; 	 (close-port f) ***
;;; 	 data)) ***

(define rundir "Tests")
(define testprefix "test")
(define Nruns 40)


(define args (command-line))
(define argc (length args))

;; ./postprocessing [N [rundir [test-]]]

(if (> argc 1) 
	 (set! Nruns (string->number (list-ref args 1))))

(if (> argc 2) 
		(set! rundir (list-ref args 2)))

(if (> argc 3) 
		(set! testprefix (list-ref args 3)))

(define (contaminant-data filename)
  (if (file-exists? filename)
		(let ((d (with-input-from-file filename (lambda () (read)))))
;				 (delete-columns d '(0 2 6 7 8)) ;; keep: t cont level N
				 (delete-columns d '(0 2 3 5 6 7 8)) ;; keep: t level
				 )
		(begin
		  (printf "the file %s does not exist\n" filename)
		  (#f "The file " filename " doesn't exist\n"))
))

(define (load-data files)
  (let ((data (map (lambda (x) (contaminant-data x)) files))
		  )
	 (let loop ((raw (cdr data)) ;; merges all the data from the same timestep into a list
					(merged (car data))
					)
		(if (null? raw)
			 merged
			 (if (null? (car raw))
				  (loop (cdr raw) merged)
				  (loop (cdr raw) (append-column merged (only-columns (car raw) `(1)))))
			 ))))
	 

(define (range start finish step . zerofill)
  (set! step (abs step))
  (cond
	((eq? start finish)
	 (list start))
	((zero? step)
	 #f)
	((> start finish) 
	 (let loop ((i start)
					(return '()))
		(if (>= i finish) 
			 (loop (- i step) (cons i return))
			 (reverse return))))

	(#t 
	 (let loop ((i start)
					(return '()))
		(if (<= i finish) 
			 (loop (+ i step) (cons i return))
			 (reverse return))))
	))


(define ind-files #f)
(define mig-files #f)
(define pop-files #f)
(define I #f)
(define M #f)
(define P #f)

(define (maybe-write I filename)
  (if (not (file-exists? filename))
		(with-output-to-file filename
		  (lambda () (write I)))))


(define (maybe-read really-read filename)
	 (if (file-exists? filename)
		  (with-input-from-file filename 
			 (lambda () (read)))
		  (really-read)))
  

(define (caching-load files filename)
  (let ((I #f))
	 (set! I (maybe-read (lambda () (load-data files)) filename))
	 (maybe-write I filename)
	 I
	 ))

(define (caching-calculate evaluate filename)
  (let ((I #f))
	 (set! I (maybe-read (lambda () (evaluate)) filename))
	 (maybe-write I filename)
	 I
	 ))

;; expects a list which contains lists of the form (t v1 v2 ...) where vi are the values associated with t
(define (mean I) (map (lambda (x) (cons (car x) (list (/ (apply + (cdr x)) (length (cdr x)))))) I))

;; expects x to be a list like that expected by (mean).  m ought to be a list of lists, each of which is in effect
;; of length 2 and has t as its first ordinate and the "mean" for the variance in the second.  
;; This is "variance relative to the second arg"
(define (varel x m) (let ((p (cdr x)) (q (cadr m)))  (/ (apply + (map (lambda (r) (* (- r q) (- r q))) p)) (length p))))
(define (value x) (cadr x))

;; apply can only handle 8192 args in a list argument, so ...

(define (apply* op L)
  (let loop ((value '())
				 (l L)
				 )
	 (if (<= (length l) 8192)
		  (if (not (null? value)) 
				(apply op (append value (list (apply op l))))
				(apply op l))

		  (loop (append value (list (apply op (sublist l 0 8192)))) 
				  (sublist l 8192 (length l)) ))))

(define (Sum L)
  (let loop ((l L)
				 (sum 0)
				 )
	 (if (null? l)
		  sum
		  (loop (cdr l) (+ sum (car l))))))

(define (Var L m)
  (/ (Sum (map (lambda (x) (* x x)) (map (lambda (y) (- y m)) L)))
	  (length L)))


(define mI #f)
(define mM #f)

(define meanI #f)
(define meanM #f)
(define meanP #f)

(define maxI #f)
(define maxM #f)
(define maxmI #f)
(define maxmM #f)
(define maxP #f)

;; variance amongst the samples at each timestep
(define vI #f)
(define vM #f)
(define vIM #f)
(define vMI #f)
(define vIP #f)
(define vMP #f)

;; mean variance through time
(define mvI #f)
(define mvM #f)
(define mvIM #f)
(define mvMI #f)
(define mvIP #f)
(define mvMP #f)


;; Variance in the variance through time
(define vmvI #f)
(define vmvM #f)
(define vmvIM #f)
(define vmvMI #f)
(define vmvIP #f)
(define vmvMP #f)

(define do-individual 'undefined)
(define do-mutating 'undefined)
(define do-population 'undefined)

(define (check-files flist)
  (apply fand (map (lambda (x) (file-exists? x)) flist)))

(define (load-all-data)
  (set! ind-files (map (lambda (x) (string-append x "/indiv-nomig/aggregate-contamination"))
							  (map (lambda (x) (string-append rundir "/" testprefix "-" (number->string x)))
									 (range 1 Nruns 1))
							  ))

  (set! mig-files (map (lambda (x) (string-append x "/migrating/aggregate-contamination"))
							  (map (lambda (x) (string-append rundir "/" testprefix "-" (number->string x)))
									 (range 1 Nruns 1))
							  ))
  (set! pop-files (list (string-append rundir "/" testprefix "-pop/pop-nomig/aggregate-contamination")))

  (display "loading raw data ... ")

  (set! do-individual (check-files ind-files))
  (set! do-mutating (check-files mig-files))
  (set! do-population (check-files pop-files))

  (if do-population
		(begin
		  (display "population, ")
		  (set! P (caching-load pop-files (string-append rundir "/" "Population.sexp")))  ))
  (if do-individual
		(begin
		  (display "individuals, ")
		  (set! I (caching-load ind-files (string-append rundir "/" "Indiv.sexp"))) ))
  (if do-mutating
		(begin 
		  (display "mutating, ")
		  (set! M (caching-load mig-files (string-append rundir "/" "Mutate.sexp"))) ))

  (display "done\n")

  (display "calculating means and variances ... ")
  ;; mean amongst the samples at each timestep

  (display "means, ")
  (if do-individual
		(begin
		  (set! mI (caching-calculate (lambda () (mean I)) (string-append rundir "/" "IndivMean.sexp"))) 
		  (set! meanI (/ (apply* + (map cadr mI)) (length I)))
		  ))
  (if do-mutating
		(begin
		  (set! mM (caching-calculate (lambda () (mean M)) (string-append rundir "/" "MutateMean.sexp"))) 
		  (set! meanM (/ (apply* + (map cadr mM)) (length M)))
		  ))

  (if do-population
		  (set! meanP (/ (apply* + (map cadr P)) (length P))) )
		  
  (display "maxima, ")
  
  (if do-individual
		(begin
		  (set! maxI (apply* max (map cadr  I)))
		  (set! maxmI (apply* max (map cadr  mI))) ))

  (if do-mutating
		(begin
		  (set! maxM (apply* max (map cadr M)))
		  (set! maxmM (apply* max (map cadr mM))) ))

  (if do-population
		(set! maxP (apply* max (map cadr P))) )

  (display "variances, ")
  ;; variance amongst the samples at each timestep
  (if do-individual
		(set! vI (caching-calculate (lambda () (map varel I mI)) (string-append rundir "/" "IVar.sexp"))) )
  (if do-mutating
		(set! vM (caching-calculate (lambda () (map varel M mM)) (string-append rundir "/" "MVar.sexp"))) )
  (if (and do-individual do-mutating)
		(begin
		  (set! vIM (caching-calculate (lambda () (map varel I mM)) (string-append rundir "/" "IMVar.sexp"))) 
		  (set! vMI (caching-calculate (lambda () (map varel M mI)) (string-append rundir "/" "MIVar.sexp"))) ))
  (if (and do-individual do-population)
		(set! vIP (caching-calculate (lambda () (map varel I P)) (string-append rundir "/" "IPVar.sexp"))) )
  (if (and do-mutating do-population)
		(set! vMP (caching-calculate (lambda () (map varel M P)) (string-append rundir "/" "MPVar.sexp"))) )

  (display "mean variances, ")
  ;; mean variance through time, then variance in the variance through time

  (if do-individual
		(begin
		  (set! mvI (/ (Sum vI) (length vI))) 
		  (set! vmvI (Var vI mvI))
		  ))
  (if do-mutating
		(begin
		  (set! mvM (/ (Sum vM) (length vM))) 
		  (set! vmvM (Var vM mvM))
		  ))
  (if (and do-individual do-mutating)
		(begin
		  (set! mvIM (/ (Sum vIM) (length vIM))) 
		  (set! mvMI (/ (Sum vMI) (length vMI))) 
		  (set! vmvIM (Var vIM mvIM))
		  (set! vmvMI (Var vMI mvMI))
		  ))
  (if (and do-individual do-population)
		(begin
		  (set! mvIP (/ (Sum vIP) (length vIP))) 
		  (set! vmvIP (Var vIP mvIP))
		  ))
  (if (and do-mutating do-population)
		(begin
		  (set! mvMP (/ (Sum vMP) (length vMP))) 
		  (set! vmvMP (Var vMP mvMP))
		  ))

  (display "done\n")
)

(define (n2) (newline)(newline))
(define (do-output)
  (with-output-to-file (string-append rundir "/" "Comparison.results")
	 (lambda ()
		(display "Number of timesteps: ")
		(if do-individual (write (list 'Ind: (length I))))
		(if do-mutating (write (list 'Mut: (length M))))
		(if do-population (write (list 'Pop: (length P))))
		(n2)

		(display "Max of the values at each step through time\n")
		(if do-individual (printf "%s %f\n" "  for individuals " maxI))
		(if do-mutating (printf "%s %f\n" "  for mutating agents " maxM))
		(if do-population (printf "%s %f\n" "  for the population " maxP))
		(newline)

		(display "Mean of the mean values at each step through time\n")
		(if do-individual (printf "%s %f\n" "  for individuals " meanI))
		(if do-mutating (printf "%s %f\n" "  for mutating agents " meanM))
		(if do-population (printf "%s %f\n" "  for the population " meanP))
		(newline)

		(display "Max of the mean values at each step through time\n")
		(if do-individual (printf "%s %f\n" "  for individuals " maxmI))
		(if do-mutating (printf "%s %f\n" "  for mutating agents " maxmM))
		(newline)
		(newline)

		(display "Maximum variance amongst the values through time\n")
		(if do-individual (printf "%s %f\n" "  for individuals " (apply* max vI)))
		(if do-mutating (printf "%s %f\n" "  for mutating agents " (apply* max vM)))
		(if (and do-individual do-mutating) 
			 (begin
				(printf "%s %f\n" "  for individuals with respect to mutating agents " (apply* max vIM))
				(printf "%s %f\n" "  for mutating agents with respect to individuals " (apply* max vMI)) ))
		(if (and do-individual do-population) (printf "%s %f\n" "  for individuals with respect to populations " (apply* max vIP)))
		(if (and do-mutating do-population) (printf "%s %f\n" "  for mutating agents with respect to populations " (apply* max vMP)))
		(newline)
		
		(display "Sum of the variance amongst the values at each step through time\n")
		(if do-individual (printf "%s %f\n" "  for individuals " (Sum vI)))
		(if do-mutating (printf "%s %f\n" "  for mutating agents " (Sum vM)))
		(if (and do-individual do-mutating) 
			 (begin
				(printf "%s %f\n" "  for individuals with respect to mutating agents " (Sum vIM))
				(printf "%s %f\n" "  for mutating agents with respect to individuals " (Sum vMI)) ))
		(if (and do-individual do-population) (printf "%s %f\n" "  for individuals with respect to populations " (Sum vIP)))
		(if (and do-mutating do-population) (printf "%s %f\n" "  for mutating agents with respect to populations " (Sum vMP)))
		(newline)

;;		(display "Mean of the variance amongst the values at each step through time\n")
;;		(display "  for individuals ")
;;		(write mvI)
;;		(newline)
;;		(display "  for mutating agents ")
;;		(write mvM)
;;		(newline)
;;		(display "  for individuals with respect to mutating agents ")
;;		(write vmvIM)
;;		(newline)
;;		(display "  for mutating agents with respect to individuals ")
;;		(write vmvMI)
;;		(newline)
;;		(display "  for individuals with respect to populations ")
;;		(write vmvIP)
;;		(newline)
;;		(display "  for mutating agents with respect to populations ")
;;		(write vmvMP)
;;		(newline)
;;		(newline)
;;		
;;		(display "Variance of the variance amongst the values at each step through time\n")
;;		(display "  for individuals ")
;;		(write vmvI)
;;		(newline)
;;		(display "  for mutating agents ")
;;		(write vmvM)
;;		(newline)
		))
  )

(if (not (string=? (car (command-line)) "/usr/local/Gambit-C/current/bin/gsi"))
	 (let ()
		(load-all-data)
		(do-output)
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
