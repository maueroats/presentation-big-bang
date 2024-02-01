#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require racket/format)
(require syntax/parse (for-syntax syntax/parse))

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

(struct meta (x y event ticks) #:transparent)
(define INITIAL-METADATA (meta 0 0 "" 0))

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

(define (presentation-big-bang* initial-model
                                #:initial-metadata
                                [initial-metadata INITIAL-METADATA]
                                #:draw draw-h
                                #:meta-draw [meta-draw-h meta-draw-h]
                                #:tick [tick-h dummy-tick-h]
                                #:key [key-h dummy-key-h]
                                #:mouse [mouse-h dummy-mouse-h]
                                #:magnification [mag 1]
                                #:delay [delay 0.2]
                                #:reset-tick [reset-tick RESET-TICK]
                                #:reset-key [reset-key RESET-KEY])
  (struct full (model meta) #:transparent)
  (define INITIAL (full initial-model initial-metadata))

  (define (actual-draw-h fm)
    (meta-draw-h (full-meta fm)
                 (scale mag (draw-h (full-model fm)))))

  (define (actual-tick-h fm)
    (if (and reset-tick
             (= reset-tick (meta-ticks (full-meta fm))))
        INITIAL
        (full (tick-h (full-model fm))
              (update-md-tick (full-meta fm)))))

  (define (actual-key-h fm key)
    (if (and reset-key (key=? key reset-key))
        INITIAL
        (full (key-h (full-model fm)
                     key)
              (update-md-key (full-meta fm)
                             key))))

  (define (actual-mouse-h fm x y event)
    (define x* (exact->inexact (/ x mag)))
    (define y* (exact->inexact (/ y mag)))
    (full (mouse-h (full-model fm)
                   x* y*
                   event)
          (update-md-mouse (full-meta fm)
                           x* y*
                           event)))

  (big-bang (full initial-model initial-metadata)
    (to-draw actual-draw-h)
    (on-tick actual-tick-h delay)
    (on-key actual-key-h)
    (on-mouse actual-mouse-h)))


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
          [else model]))

  (define (mh model x y event)
    (struct-copy m model [r (m-r model)]))

  (define (th model)
    (struct-copy m model [t (modulo (+ 10 (m-t model)) 150)]))

  (define (run)
   (presentation-big-bang* (m 150 90 35 100)
                            #:initial-metadata (meta 0 0 "" 0)
                            #:draw dh
                            #:tick th
                            #:key kh
                            #:mouse mh
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
    #:datum-literals (to-draw on-draw on-mouse on-tick on-key)
    [(_ initial-model:expr
        (~alt ((~or* to-draw on-draw) dh)
              (on-mouse mh)
              (on-tick th (~optional delay))
              (on-key kh)
              misc)
        ...)
     #'(presentation-big-bang* initial-model
                               (~? (~@ #:draw dh)) ...
                               (~? (~@ #:mouse mh)) ...
                               (~? (~@ #:key kh)) ...
                               (~? (~@ #:tick th)) ...
                               (~? (~@ #:delay delay)) ...             
                               (~? misc) ...)]))

