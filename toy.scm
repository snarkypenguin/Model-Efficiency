;-   Identification and Changes

;--

;	kernel.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2008.04.07
;		Location: localhost:/usr/home/gray/Study/playpen/kernel.scm.scm
;
;-  Code 


(require 'sort)
(require 'pretty-print)
(require 'common-list-functions)

(load "stats-bins.scm")

(define id 0)

;(define X (stats-bin))
;(define Y (stats-bin))


(load "sort.scm")
(load "helpers.scm")
(load "models.scm")
(load "units.scm")

(define int-random random)
(define (random . scale)
  (* (if (null? scale) 1.0 (car scale)) (random-real)))

(define random:uniform random-real)

(define time-in-model 0.0)
(define migration-time 0.0)
(define kernel-time 0.0)
(define contact-time 0.0)
;(define inner-contact-time 0.0)
(define contamination-time 0.0)
(define results '())

(load "config.scm")

;---------------------------------------------------
;             Utility routines
;---------------------------------------------------

(define (make-log filename)
  (let ((file #f)
		  (filename filename)
		  (remember #f)
		  )
;	 (set! file (open-file filename "wl"))
	 (set! file (open-output-file filename))

	 (lambda (method . args)
		(if (or file (member method '(open reopen recall dump)))
			 (case method
				((open reopen)
				 (if file (close file))
				 (set! filename (car args))
;				 (set! file (open-file filename "wl"))
				 (set! file (open-file filename))
				 )
				('remember (set! remember '()))
				('recall 
				 (if remember
					  (reverse remember)
					  #f))
				('close 
				 (close file)
				 (set! file #f)
				 )
				('add 
				 (cond 
				  ((null? args) (write-line*  "" file))
				  ((and (eq? (length args) 1) (list? (car args)))
					(write-line* (car args) file)
					(if remember (set! remember (cons (car args) remember)))
					)
				  (#t (write-line* args file)
						(if remember (set! remember (cons args remember)))
						)
				  )
				 )
				('dump 
				 (if remember
					  (pretty-print (reverse remember))
					  (dnl "There is no memory associated with '" filename "'")))
				(else (dnl "There is no method called " method " for log files"))
				)
			 (dnl "The log file '" filename "' is not open")
			 )
	 )
  )
)



;---------------------------------------------------
;              Drunkards stagger
;---------------------------------------------------


(define (circle t period radius d)
  (let ((x (* radius (cos (* 2.0 pi (/ t period)))))
		  (y (* radius (sin (* 2.0 pi (/ t period)))))
		  )
	 
	 (cond
	  ((eq? d 1) (list x))
	  ((eq? d 2) (list x y))
	  (#t (abort "circle-dimension-error")))
	 ))

; q is the proportion of the circle ... i.e in [0,1]
(define (random-deviation v q t period)
  (if (zero? q)
		v
		(rotated-velocity v (* q (random-angle))))
  )


;---------------------------------------------------
;               Kernel support
;---------------------------------------------------

;; Compares the subjective time of two agents
(define (Qcmp r1 r2)
  (< (r1 'subjective-time) (r2 'subjective-time)))

(define (map-q q arg)
  (map (lambda (s) (s arg)) q))


(define (running-queue rq stop)
  (cond
	((null? rq) #f)
	((not (pair? rq)) #f)
	((not (list? rq)) #f)
	((not (procedure? (car rq))) #f)
	(#t (let ((f (car rq)))
			(< (f 'subjective-time) stop)))))

;; returns the "time" of the agent at the head of the queue (unused)
(define (model-time rq stop)
  (if (running-queue rq stop)
		(if (or (null? rq) (not (list? rq)))
			 rq
			 (let ((f (car rq)))
				(f 'subjective-time))
			 )
		'end-of-run
		)
  )

;; returns an interval (tick-length) based on the current time, the desired tick length, 
;; the nominated end of the run and a list of target times
(define (interval t ddt stopat tlist)
  ;; tlist is a sorted queue of times to run
  (if (< (- stopat t) ddt)
		(set! ddt (- stopat t)))

  (cond
	((null? tlist)	
	 ddt)
	((and (list? tlist) 
			(number? (car tlist))
			(eq? (car tlist) t)
			)
	 ddt)
	((and (list? tlist) 
			(number? (car tlist))
			)
	 (- (car tlist) t))
	(else 'bad-time-to-run)))

;; remove stale times in the time-to-run queue
(define (prune-local-run-queue tm ttr)
  (let (
;		  (call-starts (cpu-time))
		  (r '())
		  )
	 (set! r (let loop ((l ttr))
				  (if (or (null? l)
							 (> tm (car l))
							 )
						l
						(loop (cdr l)))))
;	 (set! kernel-time (+ kernel-time (- (cpu-time) call-starts)))
	 r )
  )

;; insert a run record in the queue Q
(define (q-insert Q rec reccmp)
  (let (
;		  (call-starts (cpu-time))
		  )
	 (set! Q (remove rec Q))
	 (let* ((f (append Q (list rec)))
			  (sf (sort f reccmp))
			  )
;		(set! kernel-time (+ kernel-time (- (cpu-time) call-starts)))
		sf)
	 )
  )

;---------------------------------------------------
;           Kernel -- the main loop
;---------------------------------------------------

(define (test-queue-size rq)
  (if (> (length rq) N)
		(begin
		  (dnl "There are " (length rq) " entries in the runqueue when we expect at most " N);
		  (dnl "These entities total " (apply + (map (lambda (x) (x 'members)) rq)) " members");
		  
		  (map (lambda (me) 
					(dnl "   " (me 'representation) ":" (me 'name) " (" me ")  @ " (me 'subjective-time) " with " (me 'members))) 
				 rq)
		  (abort "Failed queue size test")
		  ))
  )


;; Runs the agent at the head of the queue
;;     The queue doesn't (and *shouldn't*) care at all about how much time the models used.
(define (queue t stop runqueue )
  (let loop ((rq runqueue))
	 (set! t (apply min (map-q rq 'subjective-time)))

	 (if (and (not (file-exists? "halt")) (< t stop) (running-queue rq stop))
		  (let ()
			 ;(Display "Running at ")(Display t)(Display #\return) 
			 (test-queue-size rq)
			 (set! rq (run-agent t stop rq))
			 (test-queue-size rq)
			 
			 (if (and (list? rq) (not (null? rq)) (not (symbol? (car rq))))
				  (loop rq)
				  rq))
		  (let ()
			 (if (file-exists? "halt")
				  (delete-file "halt"))
			 rq)
		  )
	 )
  )

;; converts the parameter vector "params" to reflect a new representation
(define (convert-params params representation)
  (let* ((newparams params)
			)
	 (list-set! newparams 3 representation)
	 (list-set! newparams 5 (if (eq? representation 'individual) 1 0))
	 newparams
	 ))


(define (distances-to what agentlist location)
  (map (lambda (agent) 
			(if (and (procedure? agent)
						(eq? (agent 'representation) what))
				 (distance location (agent 'loc))
				 2e308)
			)
		 agentlist)
)

(define (distances-to-agents agentlist location)
  (map (lambda (agent) 
			(if (procedure? agent)
				 (distance location (agent 'loc))
				 1e308)
			)
		 agentlist)
)

(define (distances-to-populations agentlist location)
  (distances-to 'population agentlist location))


;; returns the index of the left-most value
(define (min-index n-list)
  (cond 
	((not (list? n-list)) 'not-a-list)
	((null? n-list) #f)
	((not (apply and? (map number? n-list)))
	 'Non-number-entry)
	(#t
	   (let* ((k (length n-list))
				 (result (let loop ((ix 0)
										  (best #f)
										  )
							  (if (>= ix k)
									best
									(let ((n (list-ref n-list ix)))
											
									  (cond 
										((infinite? n) ;; skip invalid entries
										 (loop (1+ ix) best))
										((and (number? n)
												(or 
												 (not best)
												 (let ((b (list-ref n-list best)))
													(or (infinite? b)
														 (and (number? best) (<= n b)))))
										  )
										 (loop (1+ ix) ix))
										(#t (loop (1+ ix) best)))
									  ))) ))
		  (if (or (not result) (infinite? result) (>= result 1e308))
				#f
				result)
	))
  )
)
	

;; Dispatches a call to the agent. It also handles special requests like mutation and spawning
(define run-agent
  (let ((populist '())) ;; Remember the population list across invocations ... equiv to a "static" in C

	 {lambda (t stop rq) ;; This is the function "run-agent"
		(let* ((rq rq)
				 (process (if (and (not (null? rq)) (list? rq)) (car rq) #f))
				 )

;	 (dnl "running " t)
		  (test-queue-size rq)
		  ;; remove the agent's run request from the top of the queue
		  (set! rq (remove process rq))
		  (test-queue-size rq)
		  
		  (DNL "In run-agent")

		  (let* ((result (if (symbol? process)
									'bad-runqueue
									(process 'run t stop)
									))
					)
			 (let (
					 ;;(call-starts (cpu-time))
					 )
				(DNL "got " result)

				{cond
				 [(list? result)
				  {case (car result)
					 
					 ;; Migrate to a different model representation ===========================================================================
					 ['migrate
					  (if (not migrate)
							(let ()
							  (set! rq (q-insert rq process Qcmp)) ;; reinsert myself in the runqueue .............................................
							  (test-queue-size rq)
							  )

							;; else we *do* do migration
							(let* ((ldt (cadr result)) ;; Ok, go for a change of representation .................................................
									 (current-form (process 'representation))
									 (params (process 'parameters))
									 )
;								 (dnl "Process parameters for a " current-form " requesting " result)
;								 (dnl "    it currently has a contaminant profile of " (process 'contamination 0))
;								 (dump-params (process 'parameters))
;								 (newline)

							  {case current-form
								 ['individual ;---------------------------------------------------------------------------------------------------
								  ;; search through the run queue for a population which is close enough, if so transfer contaminant
								  ;; otherwise make a new population of *one* member.  This is inefficient for the first one, but the subsequent 
								  ;; beasties should merge into the first.  Beasties transfer themselves to the "finished" queue before they 
								  ;; disappear, lest we lose them from the system utterly.
								  
								  ;; "close enough" means within the radius of the population, when we merge the individual with the population, 
								  ;; we will have to decay the contaminants appropriately before hand so that the two are synchronous (we know that 
								  ;; the individual must be either ahead or synchronous by virtue of being at the top of the list
								  

								  (let* ((ploc (process 'loc))
											(q (if (null? populist) rq populist))
											(distances (if (null? q) #f (distances-to-populations q ploc)))
											(best-proc-ix (if distances (min-index distances) #f))
											(best-proc (if best-proc-ix (list-ref q best-proc-ix) #f))
											)
									 ;; need the "close-enough" test here
									 (if (and best-proc (best-proc 'is-a 'population))
										  (let ((dt (max 0 (- (best-proc 'subjective-time) (process 'subjective-time))))  ;;; THIS MAY NEED SOME EXTRA THOUGHT ***
												  )
											 ;;(wdnl "moving to an existing population at time " (+ t (cadr result)))
											 ;;(wdnl "my subjective time is " (process 'subjective-time))
											 ;;(wdnl "the population is at " (best-proc 'subjective-time))
											 ;;(wdnl "the target population has a contaminant profile of " (best-proc 'contamination 0))
											 (best-proc 'incorporate-contaminant (process 'contamination dt))
											 (best-proc 'increment-members (process 'members))
											 ;;(wdnl "the population now has a contaminant profile of " (best-proc 'contamination 0))
											 )
										  (let* ((pparams (convert-params params 'population))
													(new-pop (apply make-entity pparams))
													)
											 ;;(wdnl "making a new population at time " (+ t (cadr result)))
											 ;;(wdnl "my subjective time is " (process 'subjective-time))
											 ;;(wdnl "the new population has a subjective time of " (new-pop 'subjective-time))
											 (new-pop 'incorporate-contaminant (process 'contamination 0)) ;; should be no decay
											 (new-pop 'zero-members)
											 (new-pop 'increment-members (process 'members))
											 ;;(dnl "new population's parameters are:")
											 ;;(dump-params pparams)
											 ;;(wdnl "the new population has a name of " (new-pop 'name))
											 ;;(wdnl "the new population has a contaminant profile of " (new-pop 'contamination 0))

											 (set! populist (cons new-pop populist))
											 (set! rq (q-insert rq new-pop Qcmp))

											 (test-queue-size rq)
											 )
										  )
									 )
								  (if (member process rq)
										(dnl "*** The individual model " (process 'representation) ":" (process 'name) " is still in the runqueue! ***")
										)
								  ]
								 ['population ;;--------------------------------------------------------------------------------------------------
								  ;; The population should create n new individual agents and quietly disappear from the
								  ;; runqueue.  It then transfers itself to the "finished" queue so that we can dump the
								  ;; results at the end.  Each of the individuals should be either given a non-zero
								  ;; contaminant level from the contaminant vector, or zero when there are no more.


								  (let* ((n (process 'members))
											(params (process 'parameters))
											(pparams (convert-params params 'individual))
											(C (process 'contamination 0))
											)
									 (wdnl "making " n " new individuals at time " (+ t (cadr result)))
									 (wdnl "my subjective time is " (process 'subjective-time))
;										(dnl "new individuals params are:")
;										(dump-params pparams)
									 (let loop ((i 0)
													)
										(if (< i n) (list-set! pparams 4 (make-name "migrated")))
										(if (< i n) 
											 (let ((new-model (apply make-entity pparams))
													 )
												;;(wdnl "Made " (new-model 'representation) " " (new-model 'name) " with " (new-model 'members) " members")
												(cond
												 ((null? C) (new-model 'set-contamination 0.0))
												 ((number? C) (new-model 'set-contamination C))
												 ((list? C) 
												  (new-model 'set-contamination (car C))
												  (set! C (cdr C))
												  )
												 (#t (dnl "A very bad thing happened here"))
												 )
												
												(set! rq (q-insert rq new-model Qcmp))
												(test-queue-size rq)
												(loop (1+ i))
												;;(wdnl "the new individual, at time " (new-model 'subjective-time) ", has a contaminant profile of " (new-model 'contamination 0))
												)
											 ))
									 (set! populist (remove process populist))
									 )

								  (if (member process populist)
										(dnl "*** The population model " (process 'representation) ":" (process 'name) " is still in the population list! ***")
										)
								  (if (member process rq)
										(dnl "*** The population model " (process 'representation) ":" (process 'name) " is still in the runqueue! ***")
										)
								  ]
								 ['deterministic-individual ;; reinsert myself in the runqueue ...................................................
								  (set! rq (q-insert rq process Qcmp))
								  (test-queue-size rq)
								  ]
								 [else
								  (dnl "You have attempted to use the farce without your light-sabre. Go directly to gaol, do not pass go: " result)
								  ]
								 } ;; end of the case 
							  )
							) ;; end of the if test on migration
					  ;;				 (set! migration-time (+ migration-time (- (cpu-time) call-starts)))
					  ] ;; end of the migration clause

					 ;; insert spawned offspring into the system ** not implemented in make-entity
					 ['spawnlist ;;------------------------------------------------------------------------------------------------------------
					  (set! rq (q-insert rq process Qcmp))
					  (test-queue-size rq)
					  (let spawners ((s (cdr result)))
						 (if (not (null? s))
							  (let ((p (car s)))
								 (if p (set! rq (q-insert rq p Qcmp)))
								 (spawners (cdr s)))))
					  ]
					 ;;
					 } ; case
				  ;;[else (dnl result " is not a recognised request!")]
				  ] ; cond clause
				 [(symbol? result) ;;---------------------------------------------------------------------------------------------------------
				  (let ()
					 (dnl "Got " result)
					 (cons result rq)
					 )]
				 ;;[(and #t (number? result))
				 ;; (let ()
				 ;;   (dnl "ran for " (* result 24) " hours")
				 ;;   (set! rq (cdr rq))
				 ;;   (set! rq (q-insert rq process Qcmp))
				 ;;   rq)]
				 [#t
				  (set! rq (q-insert rq process Qcmp))]
				 }
				)
			 (test-queue-size rq)
			 (DNL "Finished with run-agent")
			 rq)
		  )
		}
	 )
  )
;;  end of run-agent


;---------------------------------------------------
;*************** PARAMETERS ************************
;---------------------------------------------------

; using test-migration to test the migration code, need to fix the logfiles -- they don't at the moment

;

;(define run-started-at (get-internal-real-time))
(define run-started-at (real-time))

;; usage: simulation.scm complex|[very]simple n start end 

(define relloc-sum 0)
(define relloc-sum-sqr 0)
(define points 0)

(define path-log #f)
(define relloc-log #f)
(define contamination-log #f)
(define data-log #f)
(define debug-log #f)

(define D 2) ; 1 or 2 dimensional run

(define pop-transition-border 2.0)
(define indiv-transition-border 4.0)
(define hard-transition-border 4.0)

(define run-tag "latest")

(define suppress-contamination #f)
(define optimise-pop #f)
(define migrate #f)
(define default-representation 'individual)
(define default-contact-model 'individual)
(define use-basic-attraction #f)


(define Start 0)

(define End 760)
;(define End (* 73 24 4))

(define SourceTime 10)
(define N 0)

;(define DDT 3600)
;(define DDT 180)
;(define CDT 45)

(define DDT 90)
(define CDT 90)

(define MIN-Cont 1e-9) ;; notionally 1 ppb


(define migration-radius 100000.0)

(define movement-scale 20000)
(define movement-interval (days))

(define source 'undefined)

(define source-scale 1.0)
(define plume-model 'asymmetric)
(define plume-period (days 34))
(define plume-lambda -0.005)
(define plume-limit (/ (log 1e-8) plume-lambda)) ; meters
(define migration-period (days 365))
;(define plume-displacement (+ 1 (* 0.75 (/ plume-limit migration-radius))) )
(define  plume-displacement 1.0)

(define plume2 (list plume-limit plume-limit))
(define -plume2 (map (lambda (x) (- 0 x)) plume2))

(define popspd #f)

(define wp-time 90)      ; time unit (attraction-life) associated with the attraction (kept in minutes!)
(define attraction 0.5)  ; 
(define variability 0.5) ; 0.5 seems ok
(define var-time 4)      ; mean time between changes in direction (kept in minutes!)
(define speed (m/s 4.0))

(define with-debugging #f)
(define with-path #f)
(define with-data #t)
(define with-relloc #f)
(define with-cont-log #t)

;; 40x1460
(define pop-mean '(-.31651129441804465 1.7654067716554034))
(define popvar (sqrt (* 9821306.677602613 9850817.342977043)))
;(define popvar (sqr 1822))

;; 20x1460
;;(define pop-mean '(6.024531034698804 3.348408741790143))
;;(define popvar (sqrt (* 9843015.02526666 9840047.185952859)))
;; 20x720
;(define pop-mean '(-2.334395564979206 5.251834934675061))
;(define popvar (sqrt (* 9812953.954166315 9881323.297650969)))

(define popstddev  (sqrt popvar))
(define popR (* 3 popstddev)) ;; 3136.8 is something like the stddev of the distance from  the target
(define 2pi-popvar (* 2pi popvar))

(define popR2 (list popR popR))
(define -popR2 (list (- popR) (- popR)))

(define (popR3 t) (list t popR popR))
(define (-popR3 t)(list t (- popR) (- popR)))

(define (set-stats stddev)
  (set! popstddev stddev)
  (set! popvar (sqr stddev))
  (set! popR (* 3 popstddev))
  (set! 2pi-popvar (* 2pi popvar))
  (set! popR2 (list popR popR))
  (set! -popR2 (list (- popR) (- popR)))
)

; The combination of 0.1, 0.4 and 3m/s gives an 
; effective swath about 40km across for a our migration circle

(define representation default-representation)
(define contact-model default-contact-model)

(define argv (command-line))

(define forced-batch-mode #f)

(define interactive (or (string=? (car argv) "gsi")
								(string=? (car argv) "/usr/local/Gambit-C/current/bin/gsi")
								(string=? (car argv) "/usr/bin/guile")))

(define argc (length argv))

;---------------------------------------------------
;           For making instances of models
;---------------------------------------------------

;---------------------------------------------------
;           Individuals
;---------------------------------------------------

; The organism here move (allegedly) around a great circle
;
; Need: mortality implemented, mass/growth according to VB (assumes adequate food)
;       spawning finished
;
(define (wp-strength strength dt wp-dt)
;(/ (- 1 (pow strength (1+ (floor (/ dt wp-dt))))) (1+ strength))
  (- 1 (pow (- 1 strength) (1+ (floor (/ dt wp-dt)))))
  )

;; We assume that our organism are all of unit mass and unit volume and they don't grow 
;;    ... kind of like a spherical cow, except cubic

(define (rep-variability rep variability)
	 (if (eq? representation 'individual) variability 0))

(define (rep-speed rep speed migration-radius migration-period)
	 (if (eq? representation 'individual) speed (/ (* 2.0 pi migration-radius) migration-period)))

(define (make-entity . args)
  (DNL "making an entity of type " representation)

  ;; These are the state variables which the closure we pass back refers to...
  (let* [ ;; State variables for the model
			(subjective-time 'uninitialised-subjective-time)
			(dt 'uninitialised-dt)
			(cont-dt 'uninitialised-cont-dt)

			(representation 'uninitialised-representation)
			(name 'unpinitialised-name)
			(number-of-members 'uninitialised-number-of-members)

			(location 'uninitialised-location)

			(movement-scale 'uninitialised-movement-scale)
			(movement-interval 'uninitialised-movement-interval)
			(velocity 'uninitialised-velocity)
			(speed 'uninitialised-speed)
			(variability 'uninitialised-variability)
			(vartime 'uninitialised-vartime)

			(radius 'uninitialised-radius)
			(period 'uninitialised-period)
			(strength 'uninitialised-strength)
			(wp-dt 'uninitialised-wp-dt)

			(contact-model 'uninitialised-contact-model)
			(decay-rate 'uninitialised-decay-rate)
			(uptake-rate 'uninitialised-uptake-rate)

			(time-to-run 'uninitialised-time-to-run)
			(contamination 'uninitialised-contamination)

			;; Set the parameters and the internal state variables appropriately
			;; This must match the set-parameters and set-state-variables functions

			;; Initialisation functions ... uses a standard mechanism for both "normal" instanciation and migration
			
			;; (set-parameters paramvector) sets the initial configuration state information for the model
			(set-parameters
			 (lambda (x)
				(set! subjective-time (list-ref x 0)) ;; subjective-time is automatically adjusted after all the processing is done
				(set! dt (list-ref x 1))
				(set! cont-dt (list-ref x 2))
				
				(set! representation (list-ref x 3))
				(set! name (list-ref x 4))
				(set! number-of-members (list-ref x 5))
				
				(set! location (list-ref x 6))
				
				(set! movement-scale (list-ref x 7))
				(set! movement-interval (list-ref x 8))
				(set! velocity (list-ref x 9))
				(set! speed (list-ref x 10))
				(set! variability (list-ref x 11))
				;; Note that the "vartime" slot can take a rng or the mean of the rng
				(set! vartime (let ((x (list-ref x 12)))
									 (if (number? x) (make-pprnd x) x)))
				
				(set! radius (list-ref x 13))
				(set! period (list-ref x 14))
				(set! strength (list-ref x 15))
				(set! wp-dt (list-ref x 16))
				
				(set! contact-model (list-ref x 17))
				(set! decay-rate (list-ref x 18))
				(set! uptake-rate (list-ref x 19))
				#t
				))

			;; (set-state-variables paramvector) sets the dynamic state information for the model
			(set-state-variables 
			 (lambda (x)
				(set! time-to-run (list-ref x 0))
				(set! contamination (list-ref x 1))
				#t
				))
			
			
			(set-args (set-parameters args))

			(set-state 
			 (set-state-variables
			  (list 
				'() 
				0.0 
				(acons subjective-time '(rxt ryt rxT ryT txt tyt txT tyT) '())
				(acons subjective-time '(rxt ryt rxt+dt ryt+dt rdist theta wpk) '())
				(acons subjective-time (append (list (v-length (list-op - location (source-location)))) location) '())
				(acons 
				 subjective-time 
				 (list 0 contamination (contact-model 0 0 location location)) '())
				) ))
			
			;; internal functions
			
			;; The compiled version *has* to have this after the state variables are all assigned for some odd reason
			(parameters
			 (lambda ()
				(list
				 subjective-time dt cont-dt
				 representation name number-of-members
				 location
				 movement-scale movement-interval velocity speed variability vartime
				 radius period strength wp-dt
				 contact-model decay-rate uptake-rate
				 
				 ;; internal state 
				 (list time-to-run contamination)
				 ))
			 )
			(dump-parameters 
			 (lambda ()	
				(dnl "representation: " representation)
				(dnl "name: " name)
				(dnl "subjective-time (days): " subjective-time) (dnl "time-to-run: " time-to-run)
				(dnl "dt: " dt)(dnl "cont-dt: " cont-dt)
				(dnl "location: " location) (dnl "velocity: " velocity) 
				(dnl "speed: " speed)
				(dnl "variability: " variability) (dnl "vartime (mins): " (vartime 'mean))
				(dnl "radius: " radius)	(dnl "period (days): " period)
				(dnl "strength: " strength) 
				(dnl "contamination: " contamination)
				(dnl "decay-rate: " decay-rate) (dnl "uptake-rate: " uptake-rate)
				#t
				))

			(run-at 
			 (lambda (x)
				(let* ((tq  (cons x time-to-run)))
				  (set! time-to-run (sort tq <=))
				  ))
			 )

         ;;;;;;;;;;  Below this are the bits that actually implement the dynamics in the model  ;;;;;;;;;;

			;; Somewhere (when there is a non-zero contaminant in the environment) there will be an assignment, this just calculates what it might be
			(uptake-depuration
			 (lambda (dt contact-mass C)
				(if (and (zero? contact-mass) (zero? C))
					 0.0
					 (rk4 (lambda (x y) 
							  (- (* uptake-rate contact-mass) (* decay-rate y))       ;; accumulating
							  ) dt 0.05 0 C))
				))

			;; This is the function that does the assignment
			(apply-uptake-depuration
			 (lambda (dt contact-mass C)
				(if (list? C)
					 (set! contamination (map (lambda (x) (uptake-depuration dt contact-mass x)) C))
					 (set! contamination (uptake-depuration dt contact-mass C))
					 )
				))

			;; This will add individual contaminant levels to a population's contaminant vector
			(incorporate-contaminant
			 (lambda (c)
;				(dnl "(Hey, it says here that the contaminant is " c ")")
				(if (and (not (null? c)) (list? c)) (set! c (remove-if (lambda (x) (<= (exact->inexact x) 0.0 )) c)))
				(let* ((C (if (and (not (null? c)) (list? c)) c (list c)))
						 (contn (if (and (not (null? contamination)) (list? contamination)) contamination (list contamination)))
						)
;				  (dnl "Setting " contamination " to " (append C contn))
				  (set! contamination (append C contn)))
				))

			;; The model proper.  
			(model 
			 {lambda (t ldt)
				(if (and (eq? representation 'individual) (not (eq? number-of-members 1)))
					 (abort))
				
				(if (symbol? contact-model)
					 (let ()
						(dnl "Contact model is " contact-model)
						(dnl "I think I'll just bail out with a horrible error now")
						(/ 1 0.0)))
				(DNL 'dsp* 
					  subjective-time
					  dt
					  cont-dt
					  representation
					  name
					  number-of-members
					  location
					  movement-scale
					  movement-interval
					  velocity
					  speed
					  variability
					  vartime
					  radius
					  period
					  strength
					  wp-dt
					  contact-model
					  decay-rate
					  uptake-rate
					  time-to-run
					  contamination
					  )

				(let ((current-contaminant-level (local-intensity t location))
						)
				  (if (>= current-contaminant-level MIN-Cont)
						(begin
						  (set! ldt (min ldt cont-dt))
						  (run-at (+ t ldt))
						  )
						)
				  
				  (if (not popspd) (set! popspd (/ (* 2.0 pi migration-radius) migration-period)))


				  (let [(ret
							(let* [(c (circle t period radius D))
									 (target (circle (+ t dt) period radius D))
									 (wpa (if use-basic-attraction
												 strength
												 (if (eq? representation 'individual)
													  (wp-strength strength dt wp-dt)
													  1.0))) ;; 1.0 means that the entity adheres to the circle totally -- no deviation possible
									 
									 ;; Calculate the location of the individual or centroid at the end of the tick
									 (step 
									  (unrelenting-directed-drunken-stagger 
										t period
										target 
										wpa      ;; pass corrected strength
										location velocity
										(rep-speed representation speed migration-radius migration-period) 
										(rep-variability representation variability) 
										movement-scale 
										movement-interval 
										ldt 
										(vartime 'mean) 
										vartime)
									  )
									 
									 ;; Calculate the _contact_ with the contaminant at its starting position and at its end point for the timestep
									 ;; This will be *bad* if the stride is long enough to jump the plume
									 (contact 
									  (if (or suppress-contamination (and isapop migrate ))
											0.0
											(let* ((next (car step))
													 (here (local-intensity t location))
													 (there (local-intensity (+ t ldt) next))
													 (somewhere-between (local-intensity (+ t (/ ldt 2.0)) (list-op / (list-op + location next) 2.0)))
													 (source (source-location))
													 (limit (+ (* speed dt) plume-limit (* hard-transition-border popR))) ; must be much bigger than we really think
													 (d (distance location source))
													 (nd (distance next source))
													 )
											  
											  ;;(dnl "About to dispatch to the contact model")
											  (cond 
												((and (not isapop) (> d limit) (> nd limit) )
												 ;;(dnl representation "out of range and passing on contact calculation" location source (car step) limit d nd here there 0.0)
												 (if with-debugging
													  (debug-log 'add (list representation "out of range and passing on contact calculation" location source (car step) limit d nd here there 0.0)))
												 0.0)
												((and isapop migrate) ;; If we are running a migrating model, uptake is handled by individuals
												 ;;(dnl representation "migrating passing on contact calculation" location source (car step) limit d nd here there 0.0)
												 (if with-debugging
													  (debug-log 'add (list representation "migrating passing on contact calculation" limit d nd here there 0.0)))
												 0.0)
												((and isapop (> d limit) (> nd limit) )
												 ;;(dnl representation "out of range and passing on contact calculation" location source (car step) limit d nd here there 0.0)
												 (if with-debugging
													  (debug-log 'add (list representation "out of range and passing on contact calculation" limit d nd here there 0.0)))
												 0.0)
												(#t
;															(dnl "==> diving in!")
												 (let ;((call-started (cpu-time)))
													  ()
													(let ((c (contact-model t (+ t ldt) location (car step))))
													  ;;(dnl representation "calculating contact" location source (car step) limit d nd here there c)
													  (if with-debugging
															(debug-log 'add (list representation "calculating contact" limit d nd here there c)))
;																 (set! inner-contact-time (+ inner-contact-time (- (cpu-time) call-started)))
													  c)
													)
												 )
												)
											  )
											)
									  )

									 ;; position relative to notional centroid of migration corrected for rotation
									 (prelative-loc (change-basis location target '()))
									 (relative-loc (change-basis (car step) target '()))
									 
									 (phase (apply atan relative-loc))

									 ;; Movement/distribution debugging data
									 ;(A (abeam (list-op - (car step) location) (list-op - target location) (* speed ldt)))  ;length of (project actual movement vector onto optimal movement vector) & scale by distance covered
									 ;(I (list-op + location (list-op * A (list-op - (car step) location))))
									 ;(B (/ (distance I target) (* speed ldt)))
									 ]
							  
							  ;; Debugging stuff
							  (if #f
									(begin
									  (dnl "The displacement of the organism is " (distance (car step) location))
									  (dnl "    time interval is " ldt)
									  (dnl "    speed is " speed)
									  ))

							  ;; Execute this every tick ... we want depuration, after all
							  (let [(wordy #f) (loud #f)]
								 (if wordy
									  (begin
										 (newline)
										 (dnl representation " " name " @" t)
;																		(dnl "current intensity " (local-intensity t location))
;																		(dnl "next intensity " (local-intensity (+ t ldt) (car step)))
										 (dnl "contact " contact ", ldt " ldt)

;																		(dnl "location " location ", step " step)
										 (Display contamination) (Display " --> ")
										 )
									  )

								 (let (
;										 (call-started (cpu-time))
										 )
									(if (not suppress-contamination)
										 (if loud
											  (begin
												 (map display (list "Contact = " contact ", contamination = "))
												 (write contamination)
												 (apply-uptake-depuration ldt contact contamination)
												 (display " ==> ")
												 (write contamination)
												 (newline)
												 )
											  (apply-uptake-depuration ldt contact contamination)
											  )
										 )
;									(set! contamination-time (+ contamination-time (- (cpu-time) call-started)))
									)

								 (if wordy
									  (dnl contamination)))
							  
							  ;;  Collect time series data

							  (set! relloc-sum (list-op + relloc-sum relative-loc))
							  (set! relloc-sum-sqr (list-op + relloc-sum-sqr (list-op * relative-loc relative-loc)))
							  (set! points (1+ points))

;							  (X 'add (car relative-loc))
;							  (Y 'add (cadr relative-loc))

							  (let ((me (string-append (symbol->string representation) ":" name)))
								 (if with-relloc
									  (relloc-log 'add me (+ t ldt) 
													  (car prelative-loc) (cadr prelative-loc) 
													  (car relative-loc) (cadr relative-loc) 
													  (car c) (cadr c) 
													  (car target) (cadr target) 
													  ;(car I) (cadr I)  
													  ;A B
													  ))

								 (if with-data
									  (data-log 'add 
													(append 
													 (list me (+ t ldt))
													 prelative-loc
													 relative-loc 
													 (list (v-length relative-loc)  phase wpa)
													 location
													 (list 
													  contact
													  (if (list? contamination)
															(/ (apply + contamination) number-of-members)
															contamination 
															)
													  number-of-members
													  (local-intensity t location)
													  (local-intensity (+ t (/ ldt 2.0)) (list-op * 0.5 (list-op + location (car step))))
													  )
													 )))

;								 (if (and with-path (or migrate (eq? representation 'individual)))
								 (if with-path 
									  (let ((start location)
											  (mean (list-op / (list-op + location (car step)) 2.0))
											  (end (car step))
											  )
										 (path-log 'add 
													  me (+ t ldt) 
													  (v-length (list-op - mean (source-location)))
													  (v-length (list-op - mean target))
													  start mean end target)
										 )
									  )


								 (if with-cont-log 
									  (contamination-log 'add 
																me
																(+ t ldt) 
																ldt 
																contact
																(if (eq? representation 'individual)
																	 contamination
																	 (if migrate
																		  (/ (apply + contamination) number-of-members)
																		  (if (list? contamination)
																				(#f 'this-should-not-happen)
																				contamination)))

																number-of-members
																(local-intensity t location)
																(local-intensity (+ t (/ ldt 2.0)) (list-op * 0.5 (list-op + location (car step))))
																(local-intensity (+ t ldt) (car step))
																))
								 )							  (set! location (car step))
															  (set! velocity (cadr step))
															  ldt
															  )
							) ;;  The model returns the amount of time it actually ran for
						  ]

					 ;; The model's tick is done now, adjust the subjective_time to reflect how much time it took for the tick
					 (set! subjective-time (+ t ldt))

					 ;; deal with any changes in the entity's representation, or the general configuration of the model as a whole
					 
					 (let* [(dstnc (distance location (source-location)))
							  (spd (if (eq? representation 'individual) speed popspd))
							  (range (* spd dt))
							  ]
						{case representation
						  ['population 
							(if (<  dstnc (* pop-transition-border (+ (max range popR) plume-limit)))
								 (list 'migrate ret) ;; returns a request to migrate and the amount of time it ran in the last tick
								 ret)]
						  ['individual
							(if (>=  dstnc (* indiv-transition-border (+ (max range popR) plume-limit)))
							  (list 'migrate ret) ;; returns a request to migrate and the amount of time it ran in the last tick
							  ret)]
						['deterministic-individual
						 ret]
						[else 'bad-representation]}
						)
					 )
				  
				  )

				}
			 )  ;; returns the amount of time it ran in the last tick, some request to the scheduler or an error condition
			]

	 ;; ----------------------------------------------------------------
	 ;; Now we pass back the closure which takes the methods: This is the "agent" wrapper around the model representation.
	 ;; ----------------------------------------------------------------
	 (lambda (method . args)
		;(dnl name " received " method)
		(let [(nargs (length args))
;				(call-starts (cpu-time))
				]
		  {cond 
			[(eq? nargs 0)
			 {cond
			  [(eq? method 'all-methods) '(all-methods 
													 snapshot 
													 i-am is-called is-a name representation parameters
													 subjective-time dt intercept run 
													 members zero-members increment-members set-members
													 set-contamination incorporate-contaminant 
													 loc vel speed var radius period strength)]
			  
			  [(eq? method 'snapshot) (dnl name ":" subjective-time)]
			  [(eq? method 'i-am) (list representation name)]
			  [(eq? method 'representation) representation]
			  [(eq? method 'is-a) representation]
			  [(eq? method 'parameters) (parameters)]
			  [(eq? method 'is-called) (member name args)]
			  [(eq? method 'name) name]
			  [(eq? method 'subjective-time)  subjective-time]
			  [(eq? method 'dt)  dt]
			  [(eq? method 'loc) location]
			  [(eq? method 'location) location]
			  [(eq? method 'vel) velocity]
			  [(eq? method 'velocity) velocity]
			  [(eq? method 'speed) speed]
			  [(eq? method 'var) variability]
			  [(eq? method 'radius) radius]
			  [(eq? method 'period) period]
			  [(eq? method 'strength) strength]
			  [(eq? method 'members) number-of-members]
			  ;;[(eq? method 'results) (form-record-list name)]
			  [(eq? method 'dump) (dump-parameters)]
			  [(eq? method 'zero-members) (set! number-of-members 0)]
			  [#t (dnl "There is no method " method " with no arguments")]
			  }
			 ]
			[(>= nargs 1)
			 {cond
			  [(eq? method 'is-a) (member representation args)]
			  [(eq? method 'contamination) 
;				(dnl "Adjusting the contamination of a " representation " over " (car args) ": "  contamination)
				(if (zero? (car args))
					 contamination
					 (apply-uptake-depuration (car args) 0.0 contamination)
					 )
				;; This is handled in the 'migrate clause
				;(set! migration-time (+ migration-time (- (cpu-time) call-starts)))
				]
			  [(eq? method 'set-contamination)
				(cond
				 ((not (list? args))
				  (dnl "Very bad magic setting the contamination level"))
				 ((null? args)
				  (dnl "Bad magic setting the contamination level"))
				 ((number? (car args))
				  (set! contamination (car args)))
				 ((apply and? (map number? args))
				  (set! contamination (car args)))
				 (#f 'incomprehensible-error-setting-contaminant))

				;; This is handled in the 'migrate clause
				;(set! migration-time (+ migration-time (- (cpu-time) call-starts)))
				]

			  [(eq? method 'zero-members) (set! number-of-members 0)]
			  [(eq? method 'increment-members) (set! number-of-members (+ number-of-members (if (null? args) 1 (car args))))]
			  [(eq? method 'set-members) (set! number-of-members (car args))]
			  [(eq? method 'incorporate-contaminant) (apply incorporate-contaminant args)]
			  [(eq? method 'is-inside) (<= (distance location (car args)) radius)]
			  
			  [(eq? method 'run-at)
				(if (> 1 (length args))
					 (run-at (car args))
					 'run-at--no-time-passed
					 )]
			  
			  [(eq? method 'run)
				;;(dnl "In the run method")
				(let* ((rt (car args))
						 (stop (cadr args))
						 (ttr (let () (set! time-to-run (prune-local-run-queue subjective-time time-to-run))
									  time-to-run))
						 (rdt (interval rt dt stop time-to-run))
						 )
				  
				  (DNL "... going to run for " rdt)
				  (cond
					((< subjective-time rt) 
					 (dnl name " is lost in time at " subjective-time " or " rt) 
					 'missing-time)
					((and (> rdt 0.0) 
							(>= subjective-time (+ rt  rdt)))
					 (dnlsp name "is driving a DeLorian.  Expected subjective-time to be" rt "but it was" subjective-time " and rdt =" rdt) 
					 'back-to-the-future)
					(#t
					 ;;(dnl "Passing control to the model at " rt " for " dt)
					 (let ((m (model rt rdt)))
;						(set! time-in-model (+ time-in-model (- (cpu-time) call-starts)))
						m)
					 )
					
					)
				  )
				]

			  [#t (dnl "There is no method for " method " with args " args)
					'bad-method]
			  }
			 ]
			}
		 )
		) ;; returns this lambda as the "make-entity" function
	 ) ;; end of containing let*
  )



;---------------------------------------------------
;   Setting up sub-models and loading the Queue
;---------------------------------------------------

; C/2 = C * exp(-L t)
; 0.5 = exp(-L t)
; -ln(0.5)/t = L


; Test run of organism doing a drunken walk around a circle

(define (wp-attraction representation attraction wp-time)
  (if (eq? representation 'individual) 
;		(/ (- (log (- 1.0 attraction))) (minutes wp-time))
		attraction
		1.0)  ; attractiveness of waypoints
)



;********************* This uses GLOBAL variables
(define (make-default-entity name nI)
  (make-entity
	;; 0, 1, 2
	Start
	(minutes DDT)
	(minutes CDT)
	
	;; 3, 4, 5
	representation
	name
	nI ; number of members
	
	;; 6
	(circle Start migration-period migration-radius D) ; starting point
	
	;; 7, 8, 9
	movement-scale
	movement-interval
	(random-deviation (if (eq? D 1) '(1) '(1 0))  1.0 0 migration-period) ; velocity

	;; 10, 11, 12
;	(if (eq? representation 'individual) speed (/ (* 2.0 pi migration-radius) migration-period)) ; speed
;	(if (eq? representation 'individual) variability 0)  ; variability
	speed
	variability
	var-time ; time for the prnd
	
	;; 13, 14, 15, 16
	migration-radius 
	(years 1) ; migratory period
	(wp-attraction representation attraction wp-time)
	wp-time
	
	;; 17, 18, 19
	(global-contact-model contact-model)
	(exp-decay-rate 0.66 (weeks 26)) ; depuration rate -- 10% every half year
	0.02 ;; 2% of the difference between tissue load and contact mass is absorbed, or the prop of the contact mass 
	)
  )

(define (go n)
  (set! time-in-model 0.0)
  (set! migration-time 0.0)
  (set! kernel-time 0.0)
  (set! contact-time 0.0)
;  (set! inner-contact-time 0.0)
  (set! contamination-time 0.0)

  (set-stats 3136.805) ;; take the stddev of the radius of positions ini the pop to be this number....

  (set! N n)

  (if with-debugging (set! debug-log (make-log (string-append run-tag "-debugging"))))
  (if with-relloc (set! relloc-log (make-log (string-append run-tag "-relloc"))))
  (if with-data (set! data-log (make-log (string-append run-tag "-data"))))
  (if with-path (set! path-log (make-log (string-append run-tag "-path"))))
  (if with-cont-log (set! contamination-log (make-log (string-append run-tag "-contamination"))))

  (set! movement-scale (/ movement-scale movement-interval))
  (set-local-intensity plume-model)
;  (set! run-started-at (get-internal-real-time))
  (set! run-started-at (real-time))
  (set! sl (source-location))

  (if #t
		(let* ((nI (if (eq? representation 'individual) 1 N))
				 (Q (map (lambda (numb) 
							 (DNL "making " numb)
							 (DNL (minutes DDT))
							 (DNL (minutes CDT))
							 (DNL representation)
							 (DNL numb)
							 (DNL (if (eq? representation 'individual) 1 N))
							 (DNL (circle Start migration-period migration-radius D))
							 (DNL (random-deviation (if (eq? D 1) '(1) '(1 0)) 1.0 0 migration-period) )
							 (DNL (if (eq? representation 'individual) 
										 speed 
										 (/ (* 2.0 pi migration-radius) migration-period)))
							 (DNL (if (eq? representation 'individual) variability 0) )
							 (DNL var-time)
							 (DNL migration-radius )
							 (DNL (years 1) )
							 (DNL (if (eq? representation 'individual) attraction 1.0))
							 (DNL representation)
							 (DNL (exp-decay-rate 0.8 (weeks 26)))

							 (let ((e (make-default-entity (make-name numb) nI))
									 )
								(DNL "Finished with entity " (e 'i-am))
								e)
							 )
						  (range 1 (if (eq? representation 'individual) N 1) 1)
						  )
					))
		  
		  (set! Q (queue Start End Q))
		  (set! results Q)
		  )
		)
  (if debug-log (debug-log 'close))
  (if path-log (path-log 'close))
  (if data-log (data-log 'close))
  (if relloc-log (relloc-log 'close))
  (if contamination-log (contamination-log 'close))
  )

(define (GO! N)
  (go N)

;;***** Need to be smart about entering things into the logs -- need a time/name tag so that only one entry goes in and we can check
;;***** to make sure that it is unique.

  (with-output-to-file (string-append run-tag "-times") 
	 (lambda() (configuration) 
; Guile...
;			  (dnl "Total time = " (- (get-internal-real-time) run-started-at))))
; Gambit...
			  (if (> points 0)
					(begin
					  (dnl "Number of points considered = " points)
					  (dnl "Mean location = " (list-op / relloc-sum points))
					  (dnl "Variance in location = " (list-op / relloc-sum-sqr points))
					  (dnl "Stddev in location = " (map sqrt (list-op / relloc-sum-sqr points)))
					  ))

;			  (dnl "Time in model code = " time-in-model)
;			  (dnl "Time in migration management = " migration-time)
;			  (dnl "Time in kernel = " kernel-time)
;			  (dnl "time associated within contact calculations = " inner-contact-time)
;			  (dnl "time associated with contact calculations = " contact-time)
;			  (dnl "time associated with contaminant uptake/decay calculations = " contamination-time)


			  (dnl "Elapsed real time = " (real-time))
			  (dnl "Total cpu time = " (cpu-time))
			  ))
  )

(define (test-migration n)
;  (set! End 60)
  (set! plume-model 'symmetric)
  (set! contact-model 'individual)
;  (set! representation 'population)
  (set! representation 'individual)
  (set! migrate #t)
  (GO! n)
  )  

(define (test-noopt)
  (set! End 1440)
  (set! plume-model 'symmetric)
  (set! contact-model 'population)
  (set! representation 'population)
  (set! migrate #f)
  (set! optimise-pop #f)
  
  (GO! 1)
  )  

(define (run args)
  (set! argv (strtok args " "))

  (let ((fb forced-batch-mode))
	 (set! forced-batch-mode #t)
	 (Do-Configuration)
	 (set! forced-batch-mode fb)
	 (GO! N)
	 )
)


;; x_mean = -1.86842612028813269494
;; y_mean = 1.51550734201692886457
;; x_var = 9835188.206785 ==> sqrt(x_var) = 3136.11036266024923831492
;; y_var = 9843840.833879 ==> sqrt(y_var) = 3137.48957510284008274186


{begin 
;(define *random-state* (seed->random-state (time->seconds (current-time))))
  (random-source-randomize! default-random-source)

  (if (not interactive) 
		(let ((fb forced-batch-mode))
		  (set! forced-batch-mode #t)
		  (Do-Configuration)
		  (configuration)
		  (set! forced-batch-mode fb)
		  (GO! N)
		  )
		(let ()
		  (set! local-intensity asymmetric-local-intensity)
		  (dnl "The variable 'results' will hold the vector of models which were used")
		  (dnl "(run \"population N 50 nomig symmetric end 0\") would set off a run")
		  )
		)
}

;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
