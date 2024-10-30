#lang racket
(require 2htdp/image
         2htdp/universe
         "main.rkt")

;; This file makes sure that the demo code works when it is not a submodule
;; (using module+).
;; The issue was that meta was a literal and not datum-literal

  (struct m (x y r t) #:transparent)

  (define (dh w)
    (place-image (circle (m-r w) "solid" (make-color (+ 100 (m-t w)) 0 0))
                 (m-x w) (m-y w)
                 (empty-scene 300 200)))

  (define (kh model k)
    (cond [(key=? k "up")
           (struct-copy m model [r (+ 10 (m-r model))])]
          [(key=? k "down")
           (struct-copy m model [r (+ -10 (m-r model))])]
          [(key=? k "e")
           'bad-result] ;; for testing the check-with function
          [(key=? k "q")
           (struct-copy m model [t -100])]
          [else model]))

  (define (kh-release model k)
    (cond[(key=? k "w")
          (struct-copy m model [r (+ 5 (m-r model))])]
         [(key=? k "s")
           (struct-copy m model [r (max 0 (+ -5 (m-r model)))])]
          [else
           model]))

  (define (mh model x y event)
    (struct-copy m model [r (m-r model)]))

  (define (th model)
    (struct-copy m model [t (modulo (+ 10 (m-t model)) 150)]))

  (define (cw? model)
    (and (m? model)
         (number? (m-x model))
         (number? (m-y model))
         (number? (m-r model))
         (number? (m-t model))))

  (define (should-stop? model)
    (= -100 (m-t model)))

  (presentation-big-bang (m 150 90 350 100)
                         (to-draw dh 600 600)
                         (name "Demo Presentation")
                         (on-tick th 0.2)
                         (on-key kh)
                         (on-release kh-release)
                         (on-mouse mh)
                         (check-with cw?)
                         (stop-when should-stop?)
                         (meta (reset-key "escape")
                               (reset-tick 300)
                               (magnification 2))
                         )
