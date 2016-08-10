;-  Identification and Changes

;--
;	config.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2010.06.12
;		Location: loki:/data/study-runs/playpen/config.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2010 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(define (set-args . lst)
  (set! argv lst)
  (set! argc (length argv)))


(define (set-flag f v)
  (let ((parsed-one 0)
		  (chatty #f)
;		  (chatty #t)
		  )
	 (cond 
	  ((and v (string=? f "defaulttag"))
		(set! run-tag (string-append 
							(symbol->string contact-model)
							"-" 
							(symbol->string plume-model)
							"-" 
							(number->string D)
							"d-" 
							(number->string DDT) 
							"-" 
							(number->string CDT)))
		(set! parsed-one 1))


	  ((and v (string=? f "path"))
		(set! with-path #t)
		(set! parsed-one 1))
	  ((and v (string=? f "nopath"))
		(set! with-path #f)
		(set! parsed-one 1))

	  ((and v (string=? f "relloc"))
		(set! with-relloc #t)
		(set! parsed-one 1))
	  ((and v (string=? f "norelloc"))
		(set! with-relloc #f)
		(set! parsed-one 1))

	  ((and v (string=? f "data"))
		(set! with-data #t)
		(set! parsed-one 1))
	  ((and v (string=? f "nodata"))
		(set! with-data #f)
		(set! parsed-one 1))

	  ((and v (string=? f "contlog"))
		(set! with-cont-log #t)
		(set! parsed-one 1))
	  ((and v (string=? f "nocontlog"))
		(set! with-cont-log #f)
		(set! parsed-one 1))

	  ((and v (string=? f "tag"))
		(set! run-tag v)
		(set! parsed-one 2))

	  ((and v (string=? f "debug"))
		(set! with-debugging #t)
		(set! parsed-one 1))

	  ((and v (string=? f "nodebug"))
		(set! with-debugging #f)
		(set! parsed-one 1))

	  ((and v (string=? f "plume-displacement"))
		(if chatty (dnl "Setting plume-displacement to " v))
		(set! plume-displacement (string->number v))
		(set! parsed-one 2))

	  ((and v (or (string=? f "attr") (string=? f "attraction")))
		(if chatty (dnl "Setting attraction to " v))
		(set! attraction (string->number v))
		(set! parsed-one 2))

	  ((and v (or (string=? f "var") (string=? f "variability")))
		(if chatty (dnl "Setting variability to " v))
		(set! variability (string->number v))
		(set! parsed-one 2))

	  ((and v (or (string=? f "spd") (string=? f "speed")))
		(if chatty (dnl "Setting speed to " v))
		(set! speed (string->number v))
		(set! parsed-one 2))

	  ((and v (or (string=? f "N") (string=? f "n")))
		(if chatty (dnl "Setting N to " v))
		(set! N (string->number v))
		(set! parsed-one 2))

	  ((or (string=? f "nocont") (string=? f "nocontamination" ))
		(if chatty (dnl "suppressing all contamination"))
		(set! suppress-contamination #t)
		(set! plume-model 'none)
		(set! parsed-one 1))

	  ((or (string=? f "opt") (string=? f "optpop" ))
		(if chatty (dnl "optimising population"))
		(set! optimise-pop #t)
		(set! parsed-one 1))

	  ((or (string=? f "noopt") (string=? f "nooptpop" ))
		(if chatty (dnl "not optimising population"))
		(set! optimise-pop #f)
		(set! parsed-one 1))

	  ((or (string=? f "mig") (string=? f "migrate" ) (string=? f "migrating" ) (string=? f "migration" ))
		(if chatty (dnl "Using migration"))
		(set! migrate #t)
		(set! optimise-pop #t)
		(set! parsed-one 1))

	  ((or (string=? f "nomig") (string=? f "dontmigrate" ) (string=? f "notmigrating" ) (string=? f "nomigration" ))
		(if chatty (dnl "Using migration"))
		(set! migrate #f)
		(set! parsed-one 1))

	  ((string=? f "basicWP" )
		(if chatty (dnl "Using basic attraction"))
		(set! use-basic-attraction #t)
		(set! parsed-one 1))

	  ((or (string=? f "pop" ) (string=? f "population" ) )
		(if chatty (dnl "Setting model to population mode"))
		(set! representation 'population)
		(set! contact-model 'population)
		(set! N 1)
		(set! parsed-one 1))
	  ((or (string=? f "deterministic-individual" )
			 (string=? f "detI" )
			 )
		(if chatty (dnl "Setting model to deterministic individual mode"))
		(set! representation 'deterministic-individual)
		(set! N 1)
		(if (eq? contact-model 'population)
			 (set! contact-model default-contact-model)
			 )
		(set! parsed-one 1))
	  ((or (string=? f "individual" )
			 (string=? f "ind" )
			 )
		(if chatty (dnl "Setting model to individual mode"))
		(set! representation 'individual)
		(set! parsed-one 1))

;	  ((string=? f "1d" )
;		(if chatty (dnl "Setting model to 1d mode"))
;		(set! D 1)
;		(set! plume-model 'radial)
;		(set! parsed-one 1))

;	  ((string=? f "2d" )
;		(if chatty (dnl "Setting model to 2d mode"))
;		(set! D 2)
;		(if (eq? plume-model 'radial) (set! plume-model 'symmetric))
;		(set! parsed-one 1))

	  ((string=? f "symmetric" )
		(if chatty (dnl "Setting plume model to " f))
		(set! plume-model 'symmetric)
		(set! parsed-one 1))
	  ((string=? f "asymmetric" )
		(if chatty (dnl "Setting plume model to " f))
		(set! plume-model 'asymmetric)
		(set! parsed-one 1))
;	  ((string=? f "radial" )
;		(if chatty (dnl "Setting plume model to " f))
;		(set! plume-model 'radial)
;		(set! parsed-one 1))

	  ((string=? f "complex" )
		(if chatty (dnl "Setting contact model to " f))
;		(set! representation 'individual)
		(set! contact-model 'individual)
		(set! parsed-one 1))

	  ((and v (string=? f "dt"))
		(if chatty (dnl "Setting default dt to " v))
		(set! DDT (string->number v))
		(set! parsed-one 2))

	  ((and v (string=? f "cdt"))
		(if chatty (dnl "Setting default cont-dt to " v))
		(set! CDT (string->number v))
		(set! parsed-one 2))

	  ((and v (string=? f "start"))
		(if chatty (dnl "Setting Starting time to " v))
		(set! Start (string->number v))
		(set! parsed-one 2))

	  ((and v (or (string=? f "end") (string=? f "finish")))
		(if chatty (dnl "Setting End time to " v))
		(set! End (string->number v))
		(set! parsed-one 2))
	  (#t (dnl "Unrecognised flag: " f))
	  )

	 (if chatty (dnl "Finished setting the flag"))

  	 (cond 
	  ((eq? representation 'population)
		(set! contact-model 'population)
		)
	  ((eq? representation 'deterministic-individual)
		)
	  )
	 parsed-one
	 )
  )

(define (parse-command-line)
  (if (or forced-batch-mode (not interactive))
		(begin ; compiled
		  (if (and (>= argc 2)
					  (or 
						(string=? (cadr argv) "-h")
						(string=? (cadr argv) "-help")
						(string=? (cadr argv) "--help")))
				(begin
				  (dnl "usage: simulation {flag value}")
				  (dnl "   Flags & values:")
				  (dnl "	     individual|population|detI")
				  (dnl "      2d|1d")
				  (dnl "      basicWP")
				  (dnl "      [no]mig")
				  (dnl "      spd|speed n")
				  (dnl "      var|variability n")
				  (dnl "      attr|attraction n")
				  (dnl "      [no](path|conc|relloc|data)")
				  (dnl "      asymmetric|symmetric")
				  (dnl "      complex|simple|verysimple" )
				  (dnl "      N nagents")
				  (dnl "      dt minutes")
				  (dnl "      cdt minutes")
				  (dnl "      start days")
				  (dnl "      end|finish days")
				  (dnl "      tag string")
				  (dnl "      defaulttag  [this must be the *last* option]")
				  
				  (exit 0)
				  (if (not forced-batch-mode) (exit 0))
				  ))

		  (let loop ((l (cdr argv)))
			 (if (pair? l)
				  (let* ((f (car l))
							(v (if (null? (cdr l)) #f (cadr l)))
							(count (set-flag f v)))
					 ;(dnl "processed " f " with " v " returning " count)
					 (if (and (number? count) (> count 0))
						  (loop (list-tail l count))))))
		  #f
		  )
		(begin ; interactive
		  #t
		  )
		)
  (if (> CDT DDT) (set! DDT CDT))
  )

(define (configuration)
  (dnl "# representation " representation)
  (dnl "# contact-model " contact-model)
  (dnl "# plume-model " plume-model)
  (dnl "# migrate " migrate)
  (dnl "# optimise-pop " optimise-pop)
  (dnl "# start " Start)
  (dnl "# end " End)
;  (display "# X ")(X 'list)(newline)
;  (display "# Y ")(Y 'list)(newline)
;  (dnl "# DDT " DDT)
;  (dnl "# CDT " CDT)
  (dnl "# N " N)
;  (dnl "# MIN-Cont " MIN-Cont)
;  (dnl "# migration-radius " migration-radius)
;  (dnl "# attraction " attraction)
;  (dnl "# wp-time " wp-time)
;  (dnl "# variability " variability)
;  (dnl "# speed  " speed )
  )

(define (Do-Configuration)
  (parse-command-line)
  (if interactive (configuration))
;  (configuration)
  
)

;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
