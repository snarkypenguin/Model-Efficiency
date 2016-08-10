;-  Identification and Changes

;--
;	models.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2008.08.09
;		Location: Loki:/home/gray/Study/playpen/models.scm
;

(load "maths.scm")
(load "rk4.scm")
(load "integrate.scm")


;
; What I want to do is to make the following things into closures which all their "parameter" data
;	plumes
;	intensities
;	contact models (need to recall the bits and pieces)




;********************* Numbers **********************


;(integrate% (lambda (x) 
;				  (integrate% (lambda (y) 
;									 (cartesian-population-density '(0 0) (list x y))) (- popR) popR linear-tol linear-section))
;				(- popR) popR fine-spatial-


(define linear-stride 100.0)
(define linear-tol 0.01)

(define spatial-stride 1000.0)
(define spatial-tol 0.01)

(define linear-density-scaling #f)
(define disk-density-scaling #f)

(define sl #f)  ;; 

;**************** Population shape ******************

;  Initially this looks something like (* 2.0 r (exp (- 0.0 (* r r)))), but when we spin it about the circle we divide by (* 2 pi r)

;;; sampled discretely at the endpoints of movement
(define (basic-individual-density r)
  (/ (exp (- 0.0 (* r r))) pi)
  )

;; This ought to be the integral of basic-individual-density w.r.t. r

;(define (basic-population-density r)
;  (/ (exp (- 0.0 (* r r 0.5))) sqrt2pi)
;)

;; For our given speed, variability and attractiveness
;;(define (pop-radius dt)
;;  

;;(define population-density basic-population-density)

;; circular bivariate normal pdf

;;(define (cartesian-population-density centre loc)
;;  (/ (exp (- (/ (sqr (distance centre loc)) 
;;					 (* 2. popvar))))
;;	  (* 2pi popvar))
;;)

(define (test-cartesian-population-density)
;;  (dnl 'test-cartesian-population-density)
  (integrate2d* (lambda (p) 
						(cartesian-population-density '(0 0) p))
					 -popR2 popR2 spatial-tol spatial-stride))

(define (test-contact-model function t t+dt)
  (let* ((p (source-location))
			(a (list-operator - p plume2))
			(b (list-operator + p plume2))
			)
	 (function (cons t a) (cons t+dt b))))


(define (test-contact function t t+dt)
  (let* ((p (source-location))
			(a (list-operator - p plume2))
			(b (list-operator + p plume2))
			)
	 (function t t+dt a b)
	 ))

;; (test-contact (lambda x (newline) (map (lambda (p) (write p)(display " ")) x) (newline)) 0 0.0625) 

(define (make-cartesian-population-density-function pop-mean popvar popR)
			(let* [(pop-mean pop-mean) 
					(pop-var pop-var)
					(popR2 (* 3 (sqrt popvar)))
					(-popR2 (- 0 popR2))
					(2pi-popvar (- 2pi popvar))
					(cpd-scale 1.0)
					(cpd {lambda (centre loc)
							 (/ 
							  (let ((c '(0.0 0.0))
									  (l (list-op - (list-op - loc centre) pop-mean))
									  )
								 
								 {/ [exp (- {/ (sqr (distance c l))
													(* 2. popvar)
													}
												)
											]
									 2pi-popvar 
									 }
								 )
							  cpd-scale)
							 })
					]
			  
			  (set! cpd-scale (integrate2d% (lambda (p) (cpd '(0 0) p)) -popR2 popR2 spatial-tol spatial-stride))
			  cpd)
			)

;**************** Plume shape/intensity *************

(define (1-local-intensity t v)
  1.0)

(define (0-local-intensity t v)
  0.0)

(define sqrt2 (sqrt 2))
(define invsqrt2 (/ 1.0 sqrt2))

(define (2-vector? v) (eq? (length v) 2))
(define (3-vector? v) (eq? (length v) 3))

(define (source-location)
  (circle SourceTime migration-period (* plume-displacement migration-radius) D))

(define (make-common-intensity popR plume-limit source-scale plume-period plume-decay)
  (let ((popR popR)
		  (plume-limit plume-limit)
		  (source-scale source-scale)
		  (plume-period plume-period)
		  (plume-decay plume-decay))
	 (lambda (t d)
		(if (< d (*  (+ (* 2.0 popR) plume-limit)))
			 (* source-scale 
				 (si (/ (+ 1.0 (cos (* (/ t plume-period) 2pi))) 2.0)) 
				 (exp (* plume-decay d)))
			 0.0))))

(define (make-symmetric-local-intensity location plume-limit source-scale plume-period plume-decay popR)
  (let ((sl location)
		  (common-intensity popR plume-limit source-scale plume-period plume-decay)
		  )
	 (lambda (t location)
		(if (not sl) (set! sl (source-location)))
		(if (3-vector? location) (set! location (cdr location)))

		(common-intensity t (distance location sl))
		)
	 )
  )	

(define (make-asymmetric-local-intensity location plume-limit source-scale plume-period plume-decay popR)
  (let ((sl location)
		  (common-intensity popR plume-limit source-scale plume-period plume-decay)
		  )
	 (lambda (t location)
		(if (not sl) (set! sl (source-location)))
		(if (3-vector? location) (set! location (cdr location)))
		(let ((l (map * (list sqrt2  invsqrt2) (list-op - location sl))))
		  (common-intensity t (v-length l))
		  )
		)
	 )
  )	


;********************* Movement *********************

(define (sliding-speed dt m tm s)
  (let* ((g (sqr (if (<= dt tm) (/ (- tm dt) tm) 0.0)))
			(f (sqrt (- 1.0 g)))
		  )
	 (+ (* s g) (* m f))
	 ))


(define (effective-speed mscale mdt dirvy speed v dt ndt)
	 (if (zero? dirvy)
		  speed
		  (let ((return-value 
					(if (zero? dirvy)
						 speed
						 (let* ((s (sign speed))
								  (spd (sliding-speed dt mscale mdt (abs speed)))
								  )
							(if #f (dnl "dt is " dt ", mdt is " mdt ", speed is " spd ))
							(if (or (zero? dt) (zero? spd)) 
								 0.0
								 (let* ((r 
											(/ (+ -1 (sqrt (+ 1.0 (* 4.0 dirvy dirvy spd (/ dt (minutes ndt))))))
												(* 2.0 dirvy dirvy))
											)
										  )
									(* s (min spd (/ (abs r) dt)))
									)	)))  
					)
				  )
			 return-value
			 )
		  )
)



;; For the 1d analogue we need to consider the radial distance to the plume
;;
;;              D = sqrt((x - r cos(t))^2 + (y - r sin(t))^2)
;;
;; but I am really after dD/dt when it comes to moving the organisms....
;; 
;;
;;             r (sin(t) (x - r cos(t)) - cos(t) (y - r sin(t)))
;;           -----------------------------------------------------
;;                                   2                 2 1/2
;;                    ((x - r cos(t))  + (y - r sin(t)) )
;;
;;
;;
;;/           / 2 t pi \ /          / 2 t pi \ \                 / 2 t pi \ /          / 2 t pi \ \ \
;;| 4 r pi sin| ------ | | x - r cos| ------ | |       4 r pi cos| ------ | | y - r sin| ------ | | |
;;|           \    p   / \          \    p   / /                 \    p   / \          \    p   / / |
;;| -------------------------------------------- -     -------------------------------------------- | 
;;\                       p                                                  p                      /
;;
;;----------------------------------------------------------------------------------------------------
;;
;;   /   / /          / 2 t pi \ \2   /          / 2 t pi \ \2 \1/2 \
;;   | 2 | | x - r cos| ------ | |  + | y - r sin| ------ | |  |    |
;;   \   \ \          \    p   / /    \          \    p   / /  /    /
;;


;;
;; The radial distance for the 1d model is currently (-r * cos(2 * pi *  / period) + x + r)
;;
;; .... I don't know why.
;;


(define (unrelenting-directed-drunken-stagger 
			t period
			wp strength loc v speed q mscale mdt dt ndt prnd)
  (cond 
;	((eq? D 1) 
;	 ;; Unidimensional run: we can't really adjust the direction vector, so we force it to adhere to the 
;	 ;; distances we expect in a 2d version.  We get really awful correspondence otherwise.
;	 (let* ((radius migration-radius)
;			  (theta (/ (* 2pi t) period))
;			  (x (car (circle SourceTime period (* plume-displacement radius) 2)))
;			  (y (cadr (circle SourceTime period (* plume-displacement radius) 2)))
;			  (swp (- x (sqrt (+ (sqr (- x  (* radius ( cos theta)))) 
;										(sqr (- y  (* radius ( sin theta)))) ))))
;			  (wv (list-op - swp loc))
;			  (spd
;				(* radius (/ (- (* (sin theta) (- x (* radius (cos theta))))
;									 (* (cos theta) (- y (* radius (sin theta)))))
;								 (sqrt
;								  (+ (sqr (- x (* radius (cos theta))))
;									  (sqr (- y (* radius (sin theta))))
;									  )))))
;			  )
;		(if (not (zero? q)) (set! speed (abs (effective-speed mscale mdt q speed v dt ndt))) )
;		(set! v (list-op / wv (v-length wv)))
;		(list (list swp) v)
;		)
;	 )

	;; This is for entities which *do not deviate* from the migratory circle such as populations
	((or (eq? strength 1.0) (zero? q))
	 (let* ((v1 (list-op - wp loc))
			  (l1 (v-length v1))
			  (v2 (if (zero? l1)
						 v
						 (list-op / v1 l1))))
		(set! v v2)
		(list wp wp v)
		))

	;; These guys, on the other hand, have downed a few bottles of bubbly....
	(#t
	 (let* (  
			  ;; All this mucking about gives us a normal distribution around the track ...

			  (r (prnd))
			  (Q (if (zero? q) 0.0 (- 1.0 (exp (* (/ r (prnd 'mean)) (log (- 1.0 q)))) )))

			  ;; added the scaling by dt to reduce or increase the effective speed in a linear fashion
			  (spd (if (not (zero? Q)) (abs (effective-speed mscale mdt Q speed v dt ndt)) 0.0))

			  (nv (random-deviation v Q t period))
			  (dv (let ((wpv (list-op - wp loc)))
					  (if (zero? (v-length wpv))
							v
							(list-op / wpv (v-length wpv)))))
			  
			  (str strength)
			  (rnv (if (< (abs str) 1.0) 
						  (list-op + (list-op * dv str) (list-op * nv (- 1.0  str)) )
						  dv))
	 )
		
		(if (and (> (v-length rnv) 0.0) (> D 1)) (set! rnv (list-op / rnv (v-length rnv)))) ; ensure the vector is of unit length
		
		;; code to avoid overshooting

		(if (<= strength 1.0)
			 (set! speed 
					 (if (> (distance loc wp) (* speed dt))
						  speed
						  (* speed (random-real))
						  )
					 )
			 )
		
		(set! v (list-op / rnv (length rnv)))
		(list  (list-op + loc (list-op * v (* speed dt))) v)
		)
	 )
	)
  )

;********************* Contact *********************

(define (distance-weight source-location location distance-limit)
  (if (and (number? source-location) (number? location))
		(vector-weight (abs (* (- source-location location) 
									  (/ 1.0 distance-limit))))
													 ;		(let ()
													 ;		  (if (number? source-location) (set! source-location (list source-location)))
													 ;		  (if (number? location) (set! location (list location)))
		(vector-weight (list-operator * (map - source-location location) 
												(/ 1.0 distance-limit))
							)
													 ;		  )
		))

;; This computes a sort of "average"
(define (make-weight-function source-loc x distance-limit)
  (lambda (x) (distance-weight source-loc x distance-limit)))


;; This computes another sort of "average"
(define (relative-path-weight source-loc loc-1 loc-2 distance-limit)
  (let* ((d (distance loc-1 loc-2))
			(p (lambda (q) (map + (map (lambda (x) (* x q)) loc-1) (map (lambda (x) (* x (- 1.0 q))) loc-2))))
			(weight-function (make-weight-function source-loc loc-1 distance-limit))
			(k (distance loc-1 loc-2))
			(K (if (zero? k) 1e-8 k))
			)
	 (/ (integrate-RV% weight-function loc-1 loc-2 linear-tol linear-stride) K)))
	 
  
; integrating along a path -------------------------------------------------------------------

; CONTACT MODEL: largely for individuals, uses average of endpoints
(define (0-global-contact-model t t+dt location-t location-t+dt)
  (#f)
  0.0)

(define (1-global-contact-model t t+dt location-t location-t+dt)
  (#f)
  1.0)

(define (pt t t0 t1 l0 l1)
  (let* ((scale (/ (- t t0) (- t1 t0)))
			(v (list-operator - l1 l0))
			)
 (list-operator + l0 (list-operator * scale v))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c and p are (t x y) triples


(define (make-pop-contact-model plume-model location plume-limit source-scale plume-period plume-decay popR))
  (let ((local-intensity (make-local-intensity plume-model local-intensity location plume-limit source-scale plume-period plume-decay popR)))
	 (lambda (c p)
		(let ((cdrp (cdr p))
				(cdrc (cdr c))
				)
		  (* (cartesian-population-density cdrc cdrp) ;; expects vectors in R2
			  (local-intensity (car c) cdrp)
			  )
		  )
		)
	 )
)

(define (make-smart-pop-contact-model plume-model location plume-limit source-scale plume-period plume-decay popR))
  (let ((pop-contact-model (make-pop-contact-model plume-model location plume-limit source-scale plume-period plume-decay popR))
		  (popR popR)
		  (plume-limit plume-limit)
		  )
	 (lambda (c p)
		(if (<= (distance (cdr c) (source-location)) (* 6 (+ popR plume-limit)))
			 (pop-contact-model c p) ;;  scales the integral of the population distribution so 99.7% is within the disk
			 0.0))
	 )
  )

;;;--- (make-*-contaminant-contact-model some-local-intensity) returns the contact model

;; CONTACT MODEL: largely for individuals through space
(define (make-individual-contaminant-contact-model plume-model local-intensity location plume-limit source-scale plume-period plume-decay popR))
  (let ((local-intensity (make-local-intensity plume-model local-intensity location plume-limit source-scale plume-period plume-decay popR)))
	 (lambda (t t+dt location-t location-t+dt)
		(if (< (abs (- t+dt t)) 1e-7)
			 0.0
			 (let* (
					  (s (v-length (list-operator - location-t+dt location-t))) ;; *&* works, but wrong
					  (result 
						(let ((available-contaminant 
								 (integrate (lambda (dt)
												  (*	(local-intensity dt (pt dt t t+dt location-t location-t+dt)) s))
												t t+dt linear-tol)))
						  (if (<= available-contaminant MIN-Cont) 0.0 available-contaminant))
						))
				result
				)
			 )
		)
	 )
  )


;; CONTACT MODEL: for populations through space
(define (make-population-contaminant-contact-model plume-model location plume-limit source-scale plume-period plume-decay popR))
  (let ((smart-pop-contact-model (make-smart-pop-contact-model plume-model location plume-limit source-scale plume-period plume-decay))
		  (popR popR)
		  )
	 (lambda (t t+dt location-t location-t+dt)
		;;  (dnl 'population-contaminant-contact-model " " t " " t+dt " " location-t " " location-t+dt)
		(if (< (abs (- t+dt t)) 1e-7)
			 0.0
			 (let ((result 
					  (let ((available-contaminant 
								(integrate-RV%	
								 (lambda (p) 
									(integrate2d%
									 (lambda (q)
										(* 2.0 (smart-pop-contact-model p (cons (car p) q)) )
										)
									 (list-op - (cdr p) (list popR popR)) (list-op + (cdr p) (list popR popR)) spatial-tol spatial-stride)
									)
								 (cons t location-t) (cons t+dt location-t+dt) spatial-tol spatial-stride)
								))
						 (if (<= available-contaminant MIN-Cont) 0.0 available-contaminant))
					  )
					 )
				result)
			 )
		)
	 )
  )

(define (make-contact-model model plume-model location plume-limit source-scale plume-period plume-decay popR))
  (case model
	 ('population (make-population-contaminant-contact-model plume-model location plume-limit source-scale plume-period plume-decay popR))
	 ('individual (make-individual-contaminant-contact-model plume-model location plume-limit source-scale plume-period plume-decay popR))
	 ('0-plume 0-global-contact-model)
	 ('1-plume 1-global-contact-model)
	 (else 
	  (dnl "global-contact-model: unrecognised model '" model "'")
	  'bad-contact-model)))
 

(make-asymmetric-local-intensity 

(define (make-local-intensity plume-model location plume-limit source-scale plume-period plume-decay popR)
  (case model
	 ;;('radial radial-local-intensity)
	 ('none (lambda y (lambda x 0.0)))
	 ('symmetric (make-symmetric-local-intensity location plume-limit source-scale plume-period plume-decay popR))
	 ('asymmetric (make-asymmetric-local-intensity location plume-limit source-scale plume-period plume-decay popR))
	 (else 
	  (dnl "global-plume-model: unrecognised model '" model "'")
	  'bad-plume-model)))




;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";; "  ***
;;; comment-end:"***" ***
;;; End: ***




