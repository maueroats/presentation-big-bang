#lang racket

#|

|#

(require 2htdp/image)
(require 2htdp/universe)
(require racket/format (for-syntax racket/format))
(require syntax/parse (for-syntax syntax/parse))

#|
;; to gain access to the pieces of a package. used in update-full.
;; struct->vector is supposed to be "less brittle"
(require (only-in rackunit require/expose))
(require/expose 2htdp/private/world
                (package-world
                 package-message))
|#

(provide presentation-big-bang)

(define TICK-DOT-LIMIT 10)
(define RESET-TICK-DEFAULT 300) ;; LOW!
(define RESET-KEY-DEFAULT "escape")
(define DELAY-DEFAULT 0.2)
(define MAGNIFICATION-DEFAULT 2)

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

;; ========================================================================

(define ((actual-draw-h mag draw-h) fm)
  (meta-draw-h (full-meta fm)
               (scale mag (draw-h (full-model fm)))))

(define ((actual-tick-h reset-tick initial-model tick-h) fm)
    (if (and reset-tick
             (= reset-tick (meta-ticks (full-meta fm))))
        initial-model
        (update-full fm
                     (tick-h (full-model fm))
                     (update-md-tick (full-meta fm)))))


(define ((actual-key-h reset-key INITIAL key-h) fm key)
    (if (and reset-key key (key=? key reset-key))
        INITIAL
        (update-full fm
                     (key-h (full-model fm)
                            key)
                     (update-md-key (full-meta fm)
                                    key))))

(define ((actual-mouse-h mag mouse-h) fm x y event)
  (define x* (exact->inexact (/ x mag)))
  (define y* (exact->inexact (/ y mag)))
  (update-full fm
               (mouse-h (full-model fm)
                        x* y*
                        event)
               (update-md-mouse (full-meta fm)
                                x* y*
                                event)))

(define ((actual-receive-h receive-h) fm msg)
  (update-full fm
               (receive-h (full-model fm)
                          msg)))

(define ((actual-check-with-f check-with-f) fm)
  (and (full? fm)
       (meta? (full-meta fm))
       (check-with-f (full-model fm))))

(define ((actual-stop-when-f stop-when-f) fm)
  (stop-when-f (full-model fm)))

(define ((actual-last-scene-f mag last-scene-f) fm)
  (if last-scene-f
      (meta-draw-h (full-meta fm)
                   (last-scene-f (full-model fm)))
      (actual-draw-h mag fm)))

(define-syntax (presentation-big-bang stx)
  (syntax-parse stx
    #:literals (to-draw on-draw on-mouse on-tick on-key on-receive
                        check-with stop-when
                        meta)
    #:datum-literals (reset-key reset-tick mag magnification)
    [(_ initial-model:expr
        (~alt (~once ((~or* to-draw on-draw) dh))
              (~optional (on-mouse mh))
              (~optional (on-tick th (~optional delay)))
              (~optional (on-key kh))
              (~optional (on-receive rh))
              (~optional (check-with ckf))
              (~optional (stop-when stop-when-f (~optional last-scene-expr)))
              (~optional (meta (~optional (reset-key RESET-KEY-LOCAL))
                               (~optional (reset-tick RESET-TICK-LOCAL))
                               (~optional ((~or* mag magnification) the-mag))))
              misc) ...)
     #'(begin
         (define mag (~? the-mag MAGNIFICATION-DEFAULT))
         (define reset-tick (~? RESET-TICK-LOCAL RESET-TICK-DEFAULT))
         (define reset-key (~? RESET-KEY-LOCAL RESET-KEY-DEFAULT))
         (define initial-full-model (full initial-model INITIAL-METADATA))
         (big-bang initial-full-model
                   (~? (on-draw (actual-draw-h mag dh)))
                   (~? (on-tick (actual-tick-h reset-tick initial-full-model th)
                                (~? delay DELAY-DEFAULT)))
                   (~? (on-mouse (actual-mouse-h mag mh)))
                   (~? (on-key (actual-key-h reset-key initial-full-model kh)))
                   (~? (on-receive (actual-receive-h rh)))
                   (~? (check-with (actual-check-with-f ckf)))
                   (~? (stop-when (actual-stop-when-f stop-when-f)
                                  (~? (actual-last-scene-f mag
                                                           last-scene-expr))))
                   (~? misc) ...))]))

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

  (presentation-big-bang (m 150 90 350 100)
                         (to-draw dh)
                         (name "Demo Presentation")
                         (on-tick th 0.2)
                         (on-key kh)
                         (on-mouse mh)
                         (check-with cw?)
                         (stop-when should-stop?)
                         (meta (reset-key "escape")
                               (reset-tick 300)
                               (magnification 2))
                         ))
