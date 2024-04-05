#lang racket

#|
WIP:
Add other expressions to make this work with universe.
Probably some metaprogramming would be smart instead of writing all the rest by hand.
|#

(require 2htdp/image)
(require 2htdp/universe)
(require racket/format)
(require syntax/parse (for-syntax syntax/parse))

#|
;; to gain access to the pieces of a package. used in update-full.
;; struct->vector is supposed to be "less brittle"
(require (only-in rackunit require/expose))
(require/expose 2htdp/private/world
                (package-world
                 package-message))
|#

(provide presentation-big-bang

         ;; optional exports
         meta
         presentation-big-bang*
         meta-draw-h)

(define TICK-DOT-LIMIT 10)
(define RESET-TICK 300) ;; LOW!
(define RESET-KEY "`")

(define (mytext t)
  (text t 32 "black"))

(define (inset d img)
  (overlay img
           (rectangle (+ d d (image-width img))
                      (+ d d (image-height img))
                      "solid"
                      "transparent")))

(define (dummy-key-h m k)
  m)
(define (dummy-mouse-h m x y event)
  m)
(define (dummy-tick-h m)
  m)
(define (dummy-receive-h m msg)
  m)
(define (dummy-check-with-f m)
  #true)
(define (dummy-stop-when-f m)
  #false)

(struct meta (x y event ticks) #:transparent)
(struct full (model meta) #:transparent)

(define INITIAL-METADATA (meta 0 0 "" 0))

(define (update-full orig new-thing [new-meta 'keep-old])
  ;; new-thing could be a model or a package containing (model, message)
  (cond [(package? new-thing)
         (define v (struct->vector new-thing))
         (define new-world (vector-ref v 1))
         (define new-message (vector-ref v 2))
         (make-package (update-full orig new-world new-meta)
                       new-message)]
        [else 
         (full new-thing
               (if (eq? new-meta 'keep-old)
                   (full-meta orig)
                   new-meta))]))

(define (unwrap-fm x)
  (full-model x))

(define (key->visible k)
  (match k
    [" "    "space"]
    ["\r"   "enter"]
    ["\b"   "backspace"]
    [_      k]))

(define (update-md-tick md)
  (struct-copy meta md [ticks (add1 (meta-ticks md))]))

(define (update-md-key md k)
  (struct-copy meta md [event (key->visible k)]))

(define (update-md-mouse md x y event)
  (struct-copy meta md [x x] [y y] [event event]))

(define (meta-draw-h md img)
  (define shown-ticks (remainder (meta-ticks md) TICK-DOT-LIMIT))
  (overlay/align "left" "top"
                 (inset 10 (mytext (format "(~a,~a)"
                                           (exact-floor (meta-x md))
                                           (exact-floor (meta-y md)))))
                 (overlay/align "right" "top"
                                (inset 10
                                       (above/align "right"
                                                    (mytext (meta-event md))
                                                    (mytext (make-string shown-ticks #\.))))
                                img)))
#|
Thinking about a better way to do this...

(define actual-mouse-h (wrap-base mouse-h (model x y event)))
->

(define (actual-mouse-h fm x y event)
  (update-model fm
                (mouse-h (full-model fm) x y event)))
|#

(define (presentation-big-bang* initial-model
                                #:initial-metadata
                                [initial-metadata INITIAL-METADATA]
                                #:draw draw-h
                                #:meta-draw [meta-draw-h meta-draw-h]
                                #:tick [tick-h dummy-tick-h]
                                #:key [key-h dummy-key-h]
                                #:mouse [mouse-h dummy-mouse-h]
                                #:receive [receive-h dummy-receive-h]
                                #:check-with [check-with-f dummy-check-with-f]
                                #:magnification [mag 1]
                                #:delay [delay 0.2]
                                #:reset-tick [reset-tick RESET-TICK]
                                #:reset-key [reset-key RESET-KEY]
                                #:stop-when [stop-when-f dummy-stop-when-f]
                                #:last-scene [last-scene-f #f]
                                #:register [register-host #f]
                                #:name [the-name "Presention World"])
  (define INITIAL (full initial-model initial-metadata))

  (define (actual-draw-h fm)
    (meta-draw-h (full-meta fm)
                 (scale mag (draw-h (full-model fm)))))

  (define (actual-tick-h fm)
    (if (and reset-tick
             (= reset-tick (meta-ticks (full-meta fm))))
        INITIAL
        (update-full (tick-h (full-model fm))
                     (update-md-tick (full-meta fm)))))

  (define (actual-key-h fm key)
    (if (and reset-key (key=? key reset-key))
        INITIAL
        (update-full (key-h (full-model fm)
                            key)
                     (update-md-key (full-meta fm)
                                    key))))

  (define (actual-mouse-h fm x y event)
    (define x* (exact->inexact (/ x mag)))
    (define y* (exact->inexact (/ y mag)))
    (update-full (mouse-h (full-model fm)
                          x* y*
                          event)
                 (update-md-mouse (full-meta fm)
                                  x* y*
                                  event)))

  (define (actual-receive-h fm msg)
    (update-full (receive-h (full-model fm)
                            msg)))

  (define (actual-check-with-f fm)
    (and (full? fm)
         (check-with-f (full-model fm))))

  (define (actual-stop-when-f fm)
    (stop-when-f (full-model fm)))

  (define (actual-last-scene-f fm)
    (if last-scene-f
        (meta-draw-h (full-meta fm)
                     (last-scene-f (full-model fm)))
        (actual-draw-h fm)))

  (if register-host
      (big-bang (full initial-model initial-metadata)
                (name the-name)
                (register register-host)
                (to-draw actual-draw-h)
                (on-tick actual-tick-h delay)
                (on-key actual-key-h)
                (on-mouse actual-mouse-h)
                (on-receive actual-receive-h)
                (check-with actual-check-with-f)
                (stop-when actual-stop-when-f actual-last-scene-f))
      ; else ... yuck
      (big-bang (full initial-model initial-metadata)
                (name the-name)
                (to-draw actual-draw-h)
                (on-tick actual-tick-h delay)
                (on-key actual-key-h)
                (on-mouse actual-mouse-h)
                (on-receive actual-receive-h)
                (check-with actual-check-with-f)
                (stop-when actual-stop-when-f actual-last-scene-f)
                )))


;; ========================================================================
;; DEMO
;; ========================================================================

(module+ main
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

  (define (run)
    (presentation-big-bang* (m 150 90 35 100)
                            #:initial-metadata (meta 0 0 "" 0)
                            #:draw dh
                            #:tick th
                            #:key kh
                            #:mouse mh
                            #:check-with cw?
                            #:stop-when should-stop?
                            #:magnification 2
                            #:delay 0.2)
    #;(save-image (scale 0.5 (meta-draw-h (meta 153.5 67.0 "move" 17)
                                          (scale 2 (dh (m 150 90 35 120)))))
                  "example.png")
    )
  (run)
  )



;; NOTE: (record? "folder") to record everything. Use a low framerate.
;;       (state #t) to show the model



(define-syntax (presentation-big-bang stx)
  (syntax-parse stx
    #:datum-literals (to-draw on-draw on-mouse on-tick on-key on-receive
                              check-with stop-when name register)
    [(_ initial-model:expr
        (~alt ((~or* to-draw on-draw) dh)
              (on-mouse mh)
              (on-tick th (~optional delay))
              (on-key kh)
              (on-receive rh)
              (check-with ckf)
              (stop-when stop-expr last-scene-expr)
              (name the-name)
              (register the-host)
              misc)
        ...)
     #'(presentation-big-bang* initial-model
                               (~? (~@ #:draw dh)) ...
                               (~? (~@ #:mouse mh)) ...
                               (~? (~@ #:key kh)) ...
                               (~? (~@ #:receive rh)) ...                               
                               (~? (~@ #:tick th)) ...
                               (~? (~@ #:delay delay)) ...
                               (~? (~@ #:check-with ckf)) ...
                               (~? (~@ #:stop-when stop-expr)) ...
                               (~? (~@ #:last-scene last-scene-expr)) ...
                               (~? (~@ #:name the-name)) ...
                               (~? (~@ #:register the-host)) ...                               
                               (~? misc) ...)]))

