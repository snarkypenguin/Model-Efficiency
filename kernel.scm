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
(load "sort.scm")
(load "helpers.scm")
(load "models.scm")
(load "units.scm")
(load "sort.scm")
(load "helpers.scm")
(load "models.scm")
(load "units.scm")

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

;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
