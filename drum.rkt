#lang typed/racket

(require math/array)

(require "synth.rkt")

(provide drum)

(define (random-sample) (- (* 2.0 (random)) 1.0))

;; Drum "samples" (Arrays of floats)
;; TODO compute those at compile-time
(define: bass-drum : (Array Float)
  (let ()
    ;; 0.05 seconds of noise whose value changes every 12 samples
    (define n-samples           (seconds->samples 0.05))
    (define n-different-samples (quotient n-samples 12))
    (for/array: #:shape (vector n-samples) #:fill 0.0
               ([i      (in-range n-different-samples)]
                [sample (in-producer random-sample (lambda _ #f))]
                #:when #t
                [j (in-range 12)]) : Float
      sample)))
(define snare
  ;; 0.05 seconds of noise
  (build-array (vector (seconds->samples 0.05))
               (lambda (x) (random-sample))))

;; Helper to avoid for macro below
(: app2 (All (A) ((Listof A) (Listof A) -> (Listof A))))
(define (app2 l1 l2)
  (append l1 l2))
(: flatten (All (A) ((Listof (Listof A)) -> (Listof A))))
(define (flatten l)
  (foldr (inst app2 A) null l))

(define-type Drum-Beat (U 'X 'O #f))

;; limited drum machine
;; drum patterns are simply lists with either O (bass drum), X (snare) or
;; #f (pause)
(: drum (Natural (Listof Drum-Beat) Integer -> (Array Float)))
(define (drum n pattern tempo)
  (define samples-per-beat (quotient (* fs 60) tempo))
  (: make-drum ((Array Float) Integer -> (Array Float)))
  (define (make-drum drum-sample samples-per-beat)
    (array-append*
     (list drum-sample
           (make-array (vector (- samples-per-beat
                                  (array-size drum-sample)))
                       0.0))))
  (define O     (make-drum bass-drum samples-per-beat))
  (define X     (make-drum snare     samples-per-beat))
  (define pause (make-array (vector samples-per-beat) 0.0))
  (array-append*
   (map (lambda: ([beat : Drum-Beat])
       (case beat
         ((X) X)
         ((O) O)
         ((#f) pause))) 
     (flatten (make-list n pattern)))
   #;(for*/list: : (Listof (Array Float))
       ([i : Number   (in-range n)]
        [beat : (U 'X 'O #f) (in-list pattern)])
     (case beat
       ((X)  X)
       ((O)  O)
       ((#f) pause)))))
;; TODO more drums, cymbals, etc.

;; replacement for for*/list macro aboce
;(lambda (beat)
;  (case beat
;    ((X) X)
;    ((O) O)
;    ((#f) pause)))
;
;(map (const pattern) (range n))


