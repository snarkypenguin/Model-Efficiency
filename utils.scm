;-  Identification and Changes

;--
;	utils.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2003.06.16
;		Location: Odin.valhalla.asgard:/home/gray/scm/utils.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2003 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

(require 'stdio)
(require 'scanf)
(require 'pretty-print)
(require 'sort)
(require 'common-list-functions)


(define (fand a . b) (if (null? b) (not (not a)) (and a (apply fand b))))
(define (for a . b) (if (null? b) (not (not a)) (or a (apply fand b))))

(define (dnlsp x . y)
  (display x)
  (map (lambda (z) 
			(display " ") 
			(if (not (atom? z)) 
				 (write z) 
				 (display z)) ) 
		 y)
  (newline))


(define (d* . args)
  (map display args)
 )

(define (dnl . args)
  (map display args)
  (newline))

(define (gcar y)
  (if (pair? y) (car y) y))

(define (gcdr y)
  (if (pair? y) (cdr y) y))


(define (o->s obj fprec)
  (if (zero? fprec)
      (object->string obj)
      (let ((s (object->string obj))
	    (prec (abs fprec)))

	(if (string? obj) (set! s obj))

	(if (< (string-length s) prec)
	    (let loop ()
	      (if (< (string-length s) prec)
		  (if (< fprec 0)
		      (set! s (string-append s " "))
		      (set! s (string-append " " s)))
		  )
	      (if (< (string-length s) prec) (loop)) ))

	(if (>  (string-length s) prec)
	    (if (< fprec 0)
		(set! s (substring s 0 prec))
		(set! s (substring s (- (string-length s) prec) (string-length s)))))
	s)))
  

(define (attrib-level level attrib-levels) 
  (let loop ((bl attrib-levels) (ix 0))
    (cond
     ((null? bl) ix)
     ((< level (gcar bl)) ix)
     (else 
      (if (list? bl) 
	  (loop (gcdr bl) (+ ix 1))
	  ix
	  ))
     )
    ))

(define (generate-matrix x-axis y-axis f)
  (let ((m '())
	(row '()))

    (let oloop ((y 0))
      (if (<= y (- (length y-axis) 1))
	  (begin 
	    (set! row '())
	    (let iloop ((x (- (length x-axis) 1)))
	      (if (>= x 0)
		  (begin
		    (set! row (cons (f (list-ref x-axis x) (list-ref y-axis y)) row))
		    (iloop (- x 1))
		    )) )
	    (set! m (cons row m))
	    (oloop (+ y 1)) )))
    m))
      

(define tm-schema '(
   "<tabular|<tformat|<table" ; preamble
   "|<row" ; row-start
   "|<cell|" ; cell-start
   "\"" ; start-quote
   "\\|" ; vinculum
   "\"" ; end-quote
   ">" ; cell-end
   ">" ; row-end
   " ----" ; first-divider
   " "; vinculum-gap
   "----" ; divider
   " " ; second-divider
   ">>>\n")) ;appendix

(define scm-schema '(
   "(tabular (tformat (table " ; preamble
   "(row " ; row-start
   "(cell " ; cell-start
   "\"" ; start-quote
   "|" ; vinculum
   "\"" ; end-quote
   ") " ; cell-end
   ") " ; row-end
   " ----" ; first-divider
   " "; vinculum-gap
   "----" ; divider
   " " ; second-divider
   ")))\n")) ;appendix

(define debug-schema '(
   "pre\n" ; preamble
   "rs: " ; row-start
   " [" ; cell-start
   ">" ; start-quote
   "|" ; vinculum
   "<" ; end-quote
   "]" ; cell-end
   " :re\n" ; row-end
   " ----" ; first-divider
   " "; vinculum-gap
   "----" ; divider
   " " ; second-divider
   "post\n")) ;appendix


(define ascii-schema '(
   "\n" ; preamble
   "" ; row-start
   "" ; cell-start
   "" ; start-quote
   "|" ; vinculum
   "" ; end-quote
   "" ; cell-end
   "\n" ; row-end
   "-----------------" ; first-divider
   " "; vinculum-gap
   "-----------------" ; divider
   "" ; second-divider
   "\n")) ;appendix


(define (print-matrix schema fprec matrix x-axis y-axis . mapping)
  (let ((preamble (list-ref schema 0))
	(row-start (list-ref schema 1))
	(cell-start (list-ref schema 2))
	(start-quote (list-ref schema 3))
	(vinculum (list-ref schema 4))
	(end-quote (list-ref schema 5))
	(cell-end (list-ref schema 6))
	(row-end (list-ref schema 7))
	(first-divider (list-ref schema 8))
	(vinculum-gap (list-ref schema 9))
	(divider  (list-ref schema 10))
	(second-divider (list-ref schema 11)) 
	(appendix (list-ref schema 12)) )
    
    (printf "%s" preamble)

    ; Print body of matrix
    (let oloop ((i matrix)
		(y (reverse y-axis)))
      (if (not (null? i))
	  (begin
	    (printf "%s" row-start)
	    ; print y-axis labels if requested
	    (if (not (null? y))
		(printf "%s%s%s%s%s%s" 
			cell-start 
			start-quote (o->s (gcar y) fprec) vinculum end-quote 
			cell-end))
	    
	    ; print row of matrix
	    (let iloop ((j (gcar i)))
	      (if (not (null? j))
		  (begin
		    (printf "%s%s%s%s%s" 
			    cell-start 
			    start-quote (o->s (if (null? mapping) (gcar j) ((car mapping) (gcar j))) fprec) end-quote 
			    cell-end)
		    (iloop (gcdr j)))))
	    (printf "%s" row-end)
	    (oloop (gcdr i) (gcdr y))
	    )))
    ; Print x-axis labels if requested
    (if (and (list? x-axis) (not (null? x-axis)))
	(begin
	  ; space for y labels
	  (if (and (list? y-axis) (not (null? y-axis)))
		(printf "%s%s%s%s%s%s" 
			cell-start 
			start-quote (o->s first-divider fprec) vinculum end-quote 
			cell-end))

	  (let aloop ((j x-axis))
	    (if (not (null? j))
		(begin
		  (printf "%s%s%s%s%s" 
			  cell-start 
			  start-quote (o->s divider fprec) end-quote 
			  cell-end)
		  (aloop (gcdr j)))))
	  (printf "%s" row-end)

	  ; space for y labels
	  (if (and (list? y-axis) (not (null? y-axis)))
	      (printf "%s%s%s%s%s%s%s" 
		      row-start 
		      cell-start start-quote (o->s second-divider fprec) vinculum-gap end-quote 
		      cell-end))

	  (let nloop ((j x-axis))
	    (if (not (null? j))
		(begin
		  (printf "%s%s%s%s%s" 
			  cell-start 
			  start-quote (o->s (gcar j)  fprec) end-quote 
			  cell-end)
		  (nloop (gcdr j)))))

	  (printf "%s" row-end)
	  ))
    (printf "%s" appendix)
    (display "")
    )
)

(define (stats-bin)
  (let ((mode 'accurate) ;; fast or accurate
		  (fmin 1e+99)
		  (fmax -1e+99)
		  (mean 0)
		  (epsilon 0.0)
		  (variance 0.0)
		  (n 0.0)
		  (ssq 0.0)
		  (sum 0.0)
		  (data '())
		  )

    (define (add-number x . i)
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

				(set! data (cons x data))
				(set! epsilon (/ (- x mean) n))

				(case mode
				  ('fast
					(set! mean (/ sum n))
					(set! variance (/ (- ssq (* n mean mean)) n) ))
				  (else ;; 'accurate
					(set! variance (+ (* (/ (- n 1) n) variance) 
											(* epsilon epsilon (- n 1))))
					(set! mean (+ epsilon mean)) )) )

			 (begin
				(set! n (+ n i))

				(set! ssq (+ ssq (* x x i)))
				(set! sum (+ sum (* x i)))

				(set! data (append (make-list i x) data))

				(case mode
				  ('fast
					(set! epsilon (/ (- x (/ (- sum x) (1- n)) n)))
					(set! mean (/ sum n))
					(set! variance (/ (- ssq (* n  mean mean)) n) ))
				  (else ;; 'accurate
					(let statloop ((j 0))
					  (if (< j i)
							(begin
							  (set! epsilon (/ (- x mean) n))
							  (set! variance (+ (* (/ (- n 1) n) variance) 
													  (* epsilon epsilon (- n 1))))
							  (set! mean (+ epsilon mean)) 
							  (statloop (1+ j))
							  )
							)
					  )
					)
				  )
				)
			 ) 
		)

	 (lambda d
		(cond
		 ((null? d)
		  (set! data (sort! data <))
		  (list 
			(cons 'stat:mode mode)
			(cons 'stat:n n) 
			(cons 'stat:mean mean)
			(if (<= 0 (length data))
				 (cons 'stat:median 0)
				 (cons 'stat:median (list-ref data (/ (length data) 2))))
			(cons 'stat:min fmin)
			(cons 'stat:max fmax)
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

		 ((symbol? (car d))
		  (case (car d)
			 ('stat:mode mode)
			 ('stat:n n) 
			 ('stat:mean mean)
			 ('stat:median (if (<= 0 (length data))
									 0
									 (list-ref data (/ (length data) 2))))
			 ('stat:min fmin)
			 ('stat:max fmax)
			 ('stat:sum_x sum)
			 ('stat:sum_x-min (- sum (* n fmin)))
			 ('stat:sum_max-x (- (* n fmax) sum))
			 ('stat:sum_xx ssq)
			 ('stat:variance variance)
			 ('stat:variance-1 (if (> n 0) (/ (* variance n) (1- n)) 0))
			 ('stat:stddev (sqrt variance))

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
		 (#t #f) 
		 )
		) 
	 )
  )


(define (void? p)
  (equal? #!void p))

(define (eof? p)
  (equal? #!eof p))

(define (bool? p)
  (or (equal? p #t) (equal? p #f)))

(define (acons key value alist)
  (cons (cons key value) alist))

(define (acons! key value alist)
  (if (null? alist)
		(cons (cons key value) '())
		(let ()
		  (set-cdr! (list-tail alist (1- (length alist))) (cons (cons key value) '()))
		  alist)))

(define (set-assoc! key value alist)
  (let ((a (assoc key alist)))
	 (if a 
		  (begin
			 (set-cdr! a value)
			 alist)
		  (acons! key value alist))))

(define (append-assoc! key value alist)
  (let ((a (assoc key alist))
		  (v (if (list? value) value (list value)))
		  )
	 (if (not a) 
		  (acons key v alist)
		  (let ((av (if (list? (cdr a)) (cdr a) (list (cdr a)))))
			 (set-assoc! key (append av v) alist)))))

;; for manipulating list elements in nasty ways ...

;(define (array-indexes array)
;  (let ((ra (apply make-array #f (array-shape array))))
;    (array-index-map! ra (lambda x x))
;    ra))

(define (list-set! l i v)
  (if (zero? i)
      (set-car! l v)
      (list-set! (cdr l) (1- i) v)))

(define list-set-car! list-set!)

(define (list-set-cdr! l i v)
  (if (zero? i)
      (set-cdr! l v)
      (list-set-cdr! (cdr l) (1- i) v)))

(define (only include list)
  (cond
	((null? list) '())
	((atom? list) list)
	((include (car list))
	 (cons (car list) (only include (cdr list))))
	(#t (only include (cdr list)))))


;; analogous to substring
(define (sublist lst first last)
  (list-tail (list-head lst last) first))


;; guarded car

(define (gcar x)
  (if (pair? x) (car x) #f))

;; guarded cdr

(define (gcdr x)
  (if (pair? x) (cdr x) #f))

;; guarded caar

(define (gcaar x)
  (if (and (pair? x) (pair? (car x))) (caar x) #f))

;; guarded cadr

(define (gcadr x)
  (if (and (pair? x) (pair? (cdr x))) (cadr x) #f))

;; guarded cdar

(define (gcdar x)
  (if (and (pair? x) (pair? (car x))) (cdar x) #f))

;; guarded cddr

(define (gcddr x)
  (if (and (pair? x) (pair? (cdr x))) (cddr x) #f))

;; guarded list-head

(define (glist-head the-list k)
  (if (and (> k 0) (not (null? the-list)))
      (cons (car the-list) (glist-head (cdr the-list) (1- k)))
      '()))

;; guarded list-tail

(define (glist-tail the-list k)
  (if (and (> k 0) (not (null? the-list)))
      (glist-tail (cdr the-list) (1- k))
      the-list))

;; guarded last

(define (glast the-list k)
  (glist-tail the-list (- (length the-list) k)))


;; (depth the-list) returns the maximum depth of the list

(define (depth l)
  (let loop ((tl l) (d 0))
    (if (not (pair? tl))
	d
	(max (loop (car tl) (1+ d)) (loop (cdr tl) d)))))


;; removes completely null strands

(define (denull l)
  (cond
   ((null? l) '())
   ((not (pair? l)) l)
   (else
    (let ((a (denull (car l)))
	  (d (denull (cdr l))))
      (cond
       ((and (null? a) (null? d)) '())
       ((null? a) d)
       (else (cons a d)))))))

(define (denull-and-flatten l)
  (cond
   ((null? l) '())
   ((not (pair? l)) l)
   (else
    (let ((a (denull (car l)))
	  (d (denull (cdr l))))
      (cond
       ((and (null? a) (null? d)) '())
       ((null? a) d)
       ((null? d) a)
       (else (cons a d)))))))



(define (level the-list n )
  (denull 
   (let loop ((tl the-list) (d 0))
     (if (not (pair? tl))
	 (if (eq? d n)
	     (list tl)
	     '())
	 (append (loop (car tl) (1+ d)) (loop (cdr tl) d))))))



;;
;; return a list of maps of lists
;;

;(define (list-map function . lists)
;  (map function lists))

(define (pair-list? l)
  (cond
   ((null? l) #t)
   ((multi-list? l) #f)
   ((double-list? l) #f)
   (#t (and (pair? (car l)) (pair-list? (cdr l))))))

(define (double-list? l)
  (cond
   ((null? l) #t)
   (#t (and (list? (car l)) (equal? (length (car l)) 2) (double-list? (cdr l))))))

(define (multi-list? l)
  (if (null? l)
      #t
      (and (list? (car l)) (>= (length (car l)) 2) (double-list? (cdr l)))))


(define (data<? m n)
  (cond
   ((symbol? m) (string<? (symbol->string m) (symbol->string n)))
   ((string? m) (string<? m n))
   ((char? m) (char<? m n))
   (#t (< m n))))

(define (basic-atom? a)
  (or (number? a) (symbol? a) (char? a) (null? a) (string? a)))


(define (atom<? a b)
  (cond 
   ((and (number? a) (number? b)) (< a b))
   ((and (string? a) (string? b)) (string<? a b))
   ((and (symbol? a) (symbol? b)) (string<? (symbol->string a) (symbol->string b)))
   ((and (char? a) (char? b)) (char<? a b))
   ((and (null? a) (not (null? b))) #t)
   ((and (not (null? a)) (null? b)) #f)
   (#t #t)
   ))


(define (list-less? a b)
  (cond
   ((and (basic-atom? a) (basic-atom? b)) 
    (atom<? a b))
   ((not (and (list? a) (list? b))) 
    (if (list? b) #t #f))
   ((null? a) #t)
   ((null? b) #f)
   (#t
    (if (and (basic-atom? (car a)) (basic-atom? (car b)))
	(if (atom<? (car a) (car b))
	    #t
	    (if (not (atom<? (car b) (car a)))
		(list-less? (cdr a) (cdr b))
		#f))
	(if (list-less? (car a) (car b))
	    #t
	    (if (not (list-less? (car b) (car a)))
		(list-less? (cdr a) (cdr b))
		#f))
	))
   ))

(define (list-head<? m n)
  (list-less? (car a) (car b)))


;;
;; The following routines perform the indicated transformations respectively
;;
;; ((a b) (c d) (a f) ... ) ->  ((a . b) (c . d) (a . f))
;; ((a . b) (c . d) (a . f)) -> ((a b) (c d) (a f) ... )
;; ((a b) (c d) (a f) ... ) ->  ((a b f ...) (c d ...))
;; ((a b f ...) (c d ...)) -> ((a b) (c d) (a f) ... )
;; ((a . b) (c . d) (a . f)) -> ((b . a) (d . c) (f . a))
;;

(define (double-list->pair-list l)
  (if (null? l) 
      '()
      (cons (cons (caar l) (cadar l)) (double-list->pair-list (cdr l)))))

(define (pair-list->double-list l)
  (if (null? l) 
      '()
      (cons (list (caar l) (cdar l)) (pair-list->double-list (cdr l)))))

(define (double-list->multi-list l)
  (define (only key l) 
    (cond
     ((null? l) '())
     ((equal? (caar l) key)
      (cons (car l) (only key (cdr l))))
     (#t (only key (cdr l)))))

;  (map car l) is a list of all heads
  (map 
   (lambda (x)
     ((lambda (lh)
	(append (list (caar lh)) (apply append (map cdr lh)))) (only x l)))
   (uniq (sort (map car l) data<?)) ) )

(define (multi-list->double-list l)
  (if (null? l) '()
      (if (null? (cdar l)) (multi-list->double-list (cdr l))
	  (cons (cons (caar l) (cons (cadar l) '()))
		(multi-list->double-list (cons (cons (caar l) (cddar l)) (cdr l)))))))



(define (reverse-pairs l)
  (if (null? l) '()
      (cons (cons (cdar l) (caar l)) (reverse-pairs (cdr l)))))


(define (indices n)
  (if (<= n 0) 
      '()
      (append (indices (1- n)) (list (1- n)))))

;;
;; only return the nth column
;;

(define (only-columns data columns)
  (map (lambda (elements)
			(members elements columns))
		 data))

;;
;; deletes columns of data
;;

(define (delete-columns data deletion-list)
  (map (lambda (elements)
	 (members elements (set-difference (indices (length elements)) deletion-list)))
       data))

;;
;; appends a column of data 
;;

(define (append-column data column)
  (map append 
       (if (and (pair? data) (pair? (car data)))
	   data
	   (map list data))
       (if (and (pair? column) (pair? (car column)))
	   column
	   (map list column))))



;;
;; returns a list of members in a list where keylist is a list of 
;; list-ref indices except that a negative index means "and all the rest from here"
;;

(define (members element keylist)
  (map (lambda (x) 
	 (if (< x 0)
	     (list-tail element (- 0 x))
	     (list-ref element x)))
       keylist))

(define (not-members element keylist)
  (set-difference element 
		  (map (lambda (x) 
			 (if (< x 0)
			     (list-tail element (- 0 x))
			     (list-ref element x)))
		       keylist)))

;;
;; perform a relational type join where key? and out? are lists of 
;; list-ref indices except that a negative index means "and all the rest from here"
;;

(define (join list1 list2 key1 key2 out1 out2)
  (define (subjoin2 l1 l2)
    (if (null? l2)
	'()
	(if (not (equal? (members l1 key1) (members (car l2) key2)))
	    (subjoin2 l1 (cdr l2))
	    (cons (append (members l1 out1) (members (car l2) out2)) 
		  (subjoin2 l1 (cdr l2))))))

  (define (subjoin1 l1 l2)
    (if (null? l1)
	'()
	(letrec ((bit (subjoin2 (car l1) l2)))
	  (if (null? bit)
	      (subjoin1 (cdr l1) l2)
	      (cons (car bit) (subjoin1 (cdr l1) l2))))))

  (subjoin1 list1 list2))


;;
;; add-ordinate  adds the ordinates to a list (makes an a-list)
;; 
;;

(define (add-ordinates value-list first . args) ; (start . (end step)) or (t-list) or (a-list)
  (cond
   ((and (list? first) (pair? (car first)))
    (let list-loop ((values value-list)(t-list first))
      (if (or (null? t-list) (null? values))
	  '()
	  (cons (cons (caar t-list) (car values)) (list-loop (cdr values) (cdr t-list))))))
   ((list? first)
    (let list-loop ((values value-list) (t-list first))
      (if (or (null? t-list) (null? values))
	  '()
	  (cons (cons (car t-list) (car values)) (list-loop (cdr values) (cdr t-list))))))
   (#t
    (let ((end first) (step 1))
      (if (> (length args) 0)
	  (set! end (list-ref args 0)))
      (if (> (length args) 1)
	  (set! step (list-ref args 1)))

      (let iterate-loop ((values value-list) (start first))
	(if (or (> start end) (null? values))
	    '()
	    (cons (cons start (car values)) (iterate-loop (cdr values) (+ start step)))))))))



;;
;; Searching
;;

(define (dfs node target nodemap carnode cdrnode)
  (if (null? node)
      #f
      (if (equal? (nodemap node) target)
	  node
	  (if (pair? node)
	      (let ((l (dfs (carnode node) target nodemap carnode cdrnode)))
		(if l
		    l
		    (dfs (cdrnode node) target nodemap carnode cdrnode)))
	      #f))))


(define (dfs-path node target nodemap carnode cdrnode)
  (if (or (null? node) (not node))
      #f
      (if (equal? (nodemap node) target)
	  (list node)
	  (if (not (pair? node))
	      #f
	      (letrec ((l (dfs-path (carnode node) target nodemap carnode cdrnode)))
		(if l
		    (if (pair? l) 
			(cons (car l) (cons 'car (cdr l)))
			(cons l 'car))

		    (letrec ((r (dfs-path (cdrnode node) target nodemap carnode cdrnode)))
		      (if r
			  (if (pair? r)
			      (cons (car r) (cons 'cdr (cdr r)))
			      (cons r 'cdr))
			  #f))))
	      ))))



(define (bfs-list key list-of-lists)
  (let bsof ((lol (list-copy list-of-lists)))
    (if (or (not (pair? lol)) (null? lol))
	#f
	(if (equal? key (car lol))
	    (car lol)
	    (let ()
	      (if (and (list? (car lol)) (list? lol))
		  (bsof (append (cdr lol) (car lol))) ; strip off a level of nesting
		  (bsof (cdr lol)) 
		  ) 
	      )
	    )
	)
    )
  )


(define (bfs key list-of-lists unwrapper)
  (let ((uw #f))
    
    (if (not (procedure? unwrapper))
	(set! uw (lambda (x) x))
	(set! uw (lambda (x) (unwrapper x)))
	)
    
    (if (null? list-of-lists)
	#f
	(let bsof ((lol (list-copy list-of-lists)))
	  (if (or (not (pair? lol)) (null? lol))
	      #f
	      (if (pair? (car lol)) 
		  (if (and (pair? lol) (equal? key (uw (car lol))))
		      (car lol)
		      (let ()
			(if (and (list? (car lol)) (list? lol))
			    (bsof (append (cdr lol) (car lol))) ; strip off a level of nesting
			    (bsof (cdr lol)) 
			    ) 
			)
		      )
		  (bsof (cdr lol)))
	      )
	  )
	)
    )
  )




(define (bfs-old node target nodemap carnode cdrnode)
  (if (or (not node) (null? node))
      #f
      (or 
       (let loop ((n node))
	 (if (or (not n) (null? n)) 
	     #f
	     (if (equal? target (nodemap n))
		 n
		 (loop (cdrnode n)))))
       (bfs (apply append (map (lambda (x) (if (pair? x) x '())) node))
	    target nodemap carnode cdrnode))))

(define (bfs-path node target nodemap carnode cdrnode)
  (if #f #t))


;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
