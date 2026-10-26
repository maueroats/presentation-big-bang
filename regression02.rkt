;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname regression02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
A specific order of the options in the meta clause was required.
This file shows that order does not matter.
|#

(require picturing-programs)
(require "main.rkt")

(define rock (circle 15 "outline" "black"))
(define bird (triangle 30 "solid" "black"))
(define wind (square 20 "solid" "orange"))
(define bg (empty-scene 400 300))

(define (ddd wind)
  (overlay/align "right" "bottom"
                 (above bird wind)
                 (overlay/align "left" "top"
                                (above wind rock)
                                bg)))


(define (tick-f img)
  (above img wind))

(presentation-big-bang wind
  (on-tick tick-f 0.5 2)
  (to-draw ddd)
  (meta (reset-key "r")
        (reset-tick 13)))

(presentation-big-bang wind
  (on-tick tick-f 0.5 2)
  (to-draw ddd)
  (meta (reset-tick 13)
        (reset-key "r")))


